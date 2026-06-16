module Tourne.Audio.Player
  ( AudioEngine
  , initAudio
  , closeAudio
  , audioCommand
  , readPlayerState
  , readVolume
  , readStreamHealth
  , readIcyMetadata
  , AudioCommand (..)
  ) where

import Relude hiding (Reader, runReader, ask, asks, local, MonadReader)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM qualified as STM
import Control.Exception.Safe (tryAny)
import Data.ByteString qualified as BS
import Data.IORef qualified as IORef
import Foreign.Ptr (nullFunPtr, nullPtr)
import Foreign.Storable (poke, peek)
import Foreign.Marshal.Alloc (alloca)

import Foreign.C.String (peekCString)
import Network.HTTP.Client (Manager)
import System.Directory (doesPathExist)
import System.Environment qualified as Env
import System.IO qualified
import Data.Text qualified
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Effectful
import Effectful.Reader.Static (Reader, runReader, ask)
import SDL qualified
import SDL.Raw.Audio qualified as SDLRaw
import SDL.Raw.Types (AudioSpec (..))

import Tourne.Types
import Tourne.Error (AppError (..), renderError)
import Tourne.Audio.Types
import Tourne.Audio.Decoder qualified as Decoder
import Tourne.Audio.Stream qualified as Stream
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold
import Tourne.Effect.Tracer (Tracer, runTracer, traceEvent)
import Tourne.Effect.HttpClient (HttpClient, runHttpClient, getManager)

import Tourne.Audio.Player.State
  ( AudioEngine (..), PlaybackEnv (..) )
import Tourne.Audio.Player.FFI
  ( c_sdl_get_queued_audio_size
  , c_sdl_pause_audio_device, c_sdl_clear_queued_audio
  , c_sdl_get_error, queueToDevice )
import Tourne.Audio.Player.Helpers (adjustVolume, drainChan)

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- | Initialize the audio system. On success, the returned
-- 'AudioEngine' is wired into a background command-processing
-- thread. The interpreter stack on that thread is
-- @runHttpClient \>\> runTracer \>\> runReader engine\>; all three
-- effects are in scope for every spawned decode action.
--
-- The HTTP 'Manager' is passed in (rather than built here) so the
-- whole program shares one 'Manager' across the API client, the
-- audio stream fetcher, and the ping checker.
initAudio :: Manager -> IO (Either AppError AudioEngine)
initAudio mgr = do
  -- Set SDL to use dummy video driver (we only need audio)
  Env.setEnv "SDL_VIDEODRIVER" "dummy"

  -- Auto-detect audio driver:
  --   - Respect user's SDL_AUDIODRIVER if already set
  --   - Otherwise try pulseaudio (common on Linux, WSL)
  envDriver <- Env.lookupEnv "SDL_AUDIODRIVER"
  case envDriver of
    Nothing -> do
      -- Detect WSL by checking for the WSLg PulseAudio socket
      wslSocket <- doesPathExist "/mnt/wslg/PulseServer"
      when wslSocket $
        Env.setEnv "PULSE_SERVER" "unix:/mnt/wslg/PulseServer"
      Env.setEnv "SDL_AUDIODRIVER" "pulseaudio"
    Just _ ->
      -- User has set their own SDL_AUDIODRIVER (pipewire, etc.)
      pure ()

  -- Initialize SDL with audio only
  SDL.initialize [SDL.InitAudio]

  -- Create communication channels
  decodedChan  <- STM.newTChanIO
  cmdChan      <- STM.newTChanIO
  stateVar     <- STM.newTVarIO Stopped
  healthVar    <- STM.newTVarIO StreamGood
  volumeVar    <- STM.newTVarIO 0.8
  rateVar      <- STM.newTVarIO 44100
  chansVar     <- STM.newTVarIO 2
  cancelVar    <- STM.newTVarIO False
  leftoverRef  <- IORef.newIORef BS.empty
  icyMetaVar   <- STM.newTVarIO Nothing  -- ICY StreamTitle sink

  -- Open SDL2 audio device in QUEUE MODE (NULL callback = no callback, use SDL_QueueAudio)
  devResult <- alloca $ \desired ->
    alloca $ \obtained -> do
      poke desired AudioSpec
        { audioSpecFreq     = 44100
        , audioSpecFormat   = 0x8010  -- AUDIO_S16SYS (Word16 AudioFormat)
        , audioSpecChannels = 2
        , audioSpecSilence  = 0
        , audioSpecSamples  = 4096
        , audioSpecSize     = 0
        , audioSpecCallback = nullFunPtr    -- NULL = queue mode
        , audioSpecUserdata = nullPtr
        }
      rawDevId <- SDLRaw.openAudioDevice nullPtr 0 desired obtained 0
      if rawDevId == 0
        then do
          sdlErr <- c_sdl_get_error >>= peekCString
          pure (Left (AudioDeviceError ("Failed to open audio device (queue mode): " <> toText sdlErr)))
        else do
          spec <- peek obtained
          let freq  = fromIntegral (audioSpecFreq spec) :: Int
              chans = fromIntegral (audioSpecChannels spec) :: Int
          pure (Right (rawDevId, freq, chans))

  case devResult of
    Left err -> pure (Left err)
    Right (devId, actualRate, actualChans) -> do
      -- Store actual device rate in the rate/channel TVars
      STM.atomically $ STM.writeTVar rateVar actualRate
      STM.atomically $ STM.writeTVar chansVar actualChans

      -- Start processing commands in background
      let engine = AudioEngine
            { aeDecodedChan   = decodedChan
            , aeCmdChan       = cmdChan
            , aeStateVar      = stateVar
            , aeStreamHealth  = healthVar
            , aeVolumeVar     = volumeVar
            , aeRateVar       = rateVar
            , aeChannelsVar   = chansVar
            , aeDeviceId      = devId
            , aeCancelToken   = cancelVar
            , aeLeftoverVar   = leftoverRef
            , aeIcyMetaVar    = icyMetaVar
            }
      tracerStart <- IORef.newIORef =<< getCurrentTime
      let tracerEnabled = debugEnabled
      _cmdThread <- async $
        runEff $ runHttpClient mgr $
        runTracer tracerStart tracerEnabled $
        runReader engine commandProcessor

      pure (Right engine)

--------------------------------------------------------------------------------
-- Command processing
--------------------------------------------------------------------------------

-- | Background command loop. Reads 'AudioCommand's from the command
-- channel and dispatches them, running 'startPlayback' for 'CmdPlay'.
-- All shared state lives in the 'AudioEngine' environment (no
-- parameter threading).
commandProcessor :: (Reader AudioEngine :> es, Tracer :> es, HttpClient :> es, IOE :> es) => Eff es ()
commandProcessor = do
  AudioEngine
    { aeCancelToken = cancelVar
    , aeCmdChan     = cmdChan
    , aeStateVar    = stateVar
    , aeDecodedChan = decodedChan
    , aeVolumeVar   = volumeVar
    , aeDeviceId    = devId
    } <- ask
  let loop = do
          cmd <- liftIO $ STM.atomically $ STM.readTChan cmdChan
          case cmd of
            CmdPlay url -> do
              -- If a previous playback is running, signal its decode
              -- loop to exit so we can start the new one. The cancel
              -- is observed by the decode loop's first action on the
              -- next iteration. The 'startPlayback' that returns
              -- will reset cancelVar back to False.
              liftIO $ STM.atomically $ STM.writeTVar cancelVar True
              liftIO $ STM.atomically $ STM.writeTVar stateVar (Connecting url)
              startPlayback url
              liftIO $ STM.atomically $ STM.writeTVar stateVar Stopped

            CmdStop -> do
              -- Same cancellation pattern as CmdPlay: the running
              -- decode loop (if any) sees cancel and exits, and the
              -- cmdThread processes the stop after startPlayback
              -- returns.
              liftIO $ STM.atomically $ STM.writeTVar cancelVar True
              liftIO $ STM.atomically $ STM.writeTVar stateVar Stopped

            CmdPause -> do
              liftIO $ SDLRaw.pauseAudioDevice devId 1
              liftIO $ STM.atomically $ STM.writeTVar stateVar Paused

            CmdResume -> do
              curVol <- liftIO $ STM.atomically $ STM.readTVar volumeVar
              liftIO $ SDLRaw.pauseAudioDevice devId 0
              liftIO $ STM.atomically $ STM.writeTVar stateVar (Playing curVol)

            CmdVolume vol ->
              liftIO $ STM.atomically $ STM.writeTVar volumeVar (min 1.0 $ max 0.0 vol)

            CmdQuit -> do
              liftIO $ STM.atomically $ STM.writeTVar cancelVar True
              liftIO $ drainChan decodedChan

          loop
  loop

-- | Start actual playback of a stream URL using queued audio.
startPlayback
  :: (Reader AudioEngine :> es, Tracer :> es, HttpClient :> es, IOE :> es)
  => Text
  -> Eff es ()
startPlayback url = do
  AudioEngine
    { aeCmdChan      = cmdChan
    , aeStateVar     = stateVar
    , aeStreamHealth = healthVar
    , aeDeviceId     = devId
    , aeCancelToken  = cancelVar
    , aeIcyMetaVar   = icyMetaVar
    } <- ask
  -- A previous playback's decode loop may have set cancelVar = True
  -- to signal the cmdThread to interrupt it. By the time we get
  -- here, the previous startPlayback has already returned, so it's
  -- safe to reset the flag for the new decode loop.
  liftIO $ STM.atomically $ STM.writeTVar cancelVar False
  -- Stream feed's debug traces are written directly to stderr from
  -- the forked IO thread, NOT routed through the Tracer effect.
  --
  -- Effectful's `withRunInIO` captures the calling thread's context
  -- via `seqUnliftIO`, which throws `HasCallStack` when called from
  -- a thread that did not start the Eff runloop. The forked feed
  -- thread is a fresh `forkIO` and has no Effectful state, so the
  -- previous code crashed inside the trace callback with:
  --
  --   If you want to use the unlifting function to run Eff
  --   computations in multiple threads, have a look at
  --   UnliftStrategy (ConcUnlift).
  --
  -- The `tryAny` in `Stream.openStream` caught the exception, set
  -- `errRef = Just "..."` and wrote `BS.empty` (EOF) to the channel.
  -- The decode loop saw EOF and exited via the `stream_end` path,
  -- leaving the player state at `Buffering 0` forever.
  --
  -- Writing directly to stderr from the fork is correct: the fork
  -- is plain IO and does not need the Eff. Gated on `debugEnabled`
  -- so production is silent unless `TOURNE_DEBUG=1` is set, which
  -- matches the gating of the in-Eff trace events.
  let streamTrace :: Text -> [Text] -> IO ()
      streamTrace l fs =
        when debugEnabled $
          System.IO.hPutStrLn System.IO.stderr
            ("F " <> toString l <> " "
              <> toString (Data.Text.intercalate " " fs))
  -- Likewise, extract the HTTP Manager from the HttpClient effect.
  httpMgr    <- getManager
  -- Reset the ICY metadata sink for the new station. The previous
  -- station's title shouldn't linger while the new stream's first
  -- metadata block arrives (which can be many seconds later).
  liftIO $ STM.atomically $ STM.writeTVar icyMetaVar Nothing
  streamResult <- liftIO $ Stream.openStream' httpMgr url streamTrace icyMetaVar
  case streamResult of
    Left err -> liftIO $ do
      STM.atomically $ STM.writeTVar stateVar (ErrorOccurred (renderError err))
      STM.atomically $ STM.writeTVar healthVar (StreamLost (renderError err))

    Right stream -> do
      decoderResult <- liftIO Decoder.mpg123Open
      case decoderResult of
        Left err -> liftIO $
          STM.atomically $ STM.writeTVar stateVar (ErrorOccurred (renderError err))

        Right decoderHandle -> do
          -- Pre-buffer target: 512 KB (~3 seconds at 44100/16/2).
          -- The decode loop will NOT unpause the SDL device until
          -- this many PCM bytes have been queued, ensuring audio
          -- starts cleanly with a meaningful buffer.
          let bufferTargetBytes = 524288

          liftIO $ STM.atomically $ STM.writeTVar stateVar (Buffering 0 bufferTargetBytes)
          liftIO $ STM.atomically $ STM.writeTVar healthVar StreamGood

          -- Drop any PCM bytes still queued from a previous
          -- playback. Without this, the new stream's first samples
          -- are appended after the old stream's tail, producing
          -- audible bleed-through and timing glitches at the
          -- switchover.
          --
          -- Pause the device first. SDL2's SDL_ClearQueuedAudio
          -- is documented as safe to call from any thread, but on
          -- a station switch the previous startPlayback deliberately
          -- leaves the device unpaused (so the cleanup_switch branch
          -- can hand off to this call without an audible pause-gap).
          -- With the device unpaused, the audio thread is actively
          -- pulling from the queue when we call clear; pausing first
          -- guarantees the audio thread is idle by the time the
          -- clear runs. The decode loop will re-unpause via its
          -- 'device_start' trace event once peBufferTarget PCM has
          -- been queued.
          _ <- liftIO $ c_sdl_pause_audio_device devId 1
          liftIO $ c_sdl_clear_queued_audio devId

          -- Concurrent MP3 buffer: a background thread reads HTTP
          -- chunks from the stream and pushes them into a TBQueue.
          -- The decode loop reads from this queue instead of blocking
          -- on network I/O, smoothing out jitter from the server's
          -- per-chunk timing (which can vary from 400-700ms).
          mp3Queue   <- liftIO $ STM.newTBQueueIO 131072  -- 128KB capacity
          readerDone <- liftIO $ STM.newTVarIO False
          let goQueue s = do
                (mChunk, rest) <- S.foldBreak Fold.one s
                case mChunk of
                  Nothing -> pure ()
                  Just c  -> do
                    STM.atomically $ STM.writeTBQueue mp3Queue c
                    goQueue rest
          readerThread <- liftIO $ async $ do
            result <- tryAny $ goQueue stream
            STM.atomically $ STM.writeTVar readerDone True
            case result of
              Left ex -> do
                STM.atomically $ STM.writeTVar stateVar (ErrorOccurred (show ex))
                STM.atomically $ STM.writeTVar healthVar (StreamLost (show ex))
              Right _ -> pure ()

          -- Per-playback environment: decoder handle + MP3 queue.
          let pbEnv = PlaybackEnv
                { peDecoderHandle = decoderHandle
                , peBufferTarget  = bufferTargetBytes
                , peMp3Queue      = mp3Queue
                , peReaderDone    = readerDone
                }

          -- Run the pull-based decode loop with the per-playback env
          -- in scope. 'started = False'; the decode loop unpauses
          -- the device once peBufferTarget PCM has been queued.
          runReader pbEnv (decodeLoopStream 0 False)

          -- Kill the background reader thread. On normal stream EOF
          -- the reader has already finished; on station-switch cancel
          -- the decode loop exited early and the reader is still
          -- blocked on the HTTP connection, so cancel it explicitly.
          liftIO $ cancel readerThread
          liftIO $ Decoder.mpg123Close decoderHandle
          -- Only pause the device on natural EOF, stop, or error.
          -- For a station switch (cancelVar set by CmdPlay), skip the
          -- pause: the next playback is about to start on the same
          -- thread and will manage the device state via its own
          -- decodeLoopStream 'started' flag.  The pending CmdPlay
          -- is still in the channel (decodeLoopStream peeks but does
          -- NOT consume it), so we detect it here.
          pendingPlay <- liftIO $ STM.atomically $ STM.tryPeekTChan cmdChan
          case pendingPlay of
            Just CmdPlay{} -> traceEvent "cleanup_switch" []
            _              -> liftIO $ SDLRaw.pauseAudioDevice devId 1

--------------------------------------------------------------------------------
-- Decode loop and chunk processing
--------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- Pull-based decode loop (Streamly)
-------------------------------------------------------------------------------

-- | Read chunks from the stream until we have at least @minBytes@ of
-- data, or until the stream ends. Returns the accumulated bytes and
-- the remaining stream.
--
-- Feeding larger batches to mpg123 is essential for throughput: the
-- HTTP 'BodyReader' returns only 1 KB per call, and the per-chunk
-- decode overhead (STM, state updates, rate limiting) becomes the
-- bottleneck when processing individual chunks — especially on slower
-- connections where each \<1 KB trickle is separated by ~400 ms.
-- | Read from the MP3 queue until we have at least @minBytes@ of
-- data, or until the reader has finished (EOF or error). The
-- background reader thread pushes each HTTP chunk into the queue
-- concurrently, so this function rarely blocks on network I/O.
gatherFromQueue :: Int -> STM.TBQueue ByteString -> STM.TVar Bool -> IO ByteString
gatherFromQueue minBytes queue doneVar = go mempty
  where
    go acc = do
      if BS.length acc >= minBytes
        then pure acc
        else do
          mChunk <- STM.atomically $ do
            done <- STM.readTVar doneVar
            if done
              then STM.tryReadTBQueue queue
              else Just <$> STM.readTBQueue queue
          case mChunk of
            Nothing -> pure acc     -- reader done + queue empty
            Just c  -> let acc' = acc <> c
                       in acc' `seq` go acc'

-- | Queue-backed decode loop. Each iteration reads from the MP3
-- queue (filled concurrently by the background reader thread),
-- decodes through mpg123, adjusts volume, and queues PCM to SDL2.
-- Unlike the previous stream-based approach, the decode loop never
-- blocks on network I/O — the background reader handles that.
--
-- The @started@ flag tracks whether the SDL device has been
-- unpaused.  The device stays paused until @peBufferTarget@ PCM
-- bytes have been queued, giving a clean initial buffer (~3 s).
--
-- Keeps the same command-peek, cancel, and rate-limiting behaviour
-- as the legacy loop so the station-switch interrupt still works.
decodeLoopStream
  :: (Reader AudioEngine :> es, Reader PlaybackEnv :> es, Tracer :> es, IOE :> es)
  => Int    -- ^ accumulated decoded bytes so far
  -> Bool   -- ^ whether the SDL device has been unpaused yet
  -> Eff es ()
decodeLoopStream accBytes started = do
  AudioEngine{..} <- ask
  PlaybackEnv{..} <- ask

  cancelled <- liftIO $ STM.atomically $ STM.readTVar aeCancelToken
  unless cancelled $ do
    peekedCmd <- liftIO $ STM.atomically $ STM.tryPeekTChan aeCmdChan
    case peekedCmd of
      Just CmdStop -> do
        traceEvent "decode_peek" ["cmd=CmdStop"]
        liftIO $ void $ STM.atomically $ STM.tryReadTChan aeCmdChan
        liftIO $ STM.atomically $ STM.writeTVar aeCancelToken True
      Just CmdQuit -> do
        traceEvent "decode_peek" ["cmd=CmdQuit"]
        liftIO $ void $ STM.atomically $ STM.tryReadTChan aeCmdChan
        liftIO $ STM.atomically $ STM.writeTVar aeCancelToken True
      Just CmdPlay{} -> do
        traceEvent "decode_peek" ["cmd=CmdPlay"]
        liftIO $ STM.atomically $ STM.writeTVar aeCancelToken True
      -- | Pause: consume the command, pause the SDL device, then
      -- block in a poll loop until Resume, Stop, Quit, Play, or Volume
      -- arrives.  The poll loop runs on the same thread as the decode
      -- loop (because commandProcessor is blocked inside us), so it
      -- reads from aeCmdChan directly via tryReadTChan.
      Just CmdPause -> do
        traceEvent "decode_peek" ["cmd=CmdPause"]
        liftIO $ void $ STM.atomically $ STM.tryReadTChan aeCmdChan
        resumed <- liftIO $ handlePause_ aeDeviceId aeCmdChan aeStateVar
                    aeVolumeVar aeCancelToken aeStreamHealth
        if resumed
          then decodeLoopStream accBytes started
          else pure ()
      -- | Resume while not paused: consume and ignore.
      Just CmdResume -> do
        traceEvent "decode_peek" ["cmd=CmdResume"]
        liftIO $ void $ STM.atomically $ STM.tryReadTChan aeCmdChan
      _ -> do
        case peekedCmd of
          Just (CmdVolume vol) -> do
            liftIO $ void $ STM.atomically $ STM.tryReadTChan aeCmdChan
            liftIO $ STM.atomically $ STM.writeTVar aeVolumeVar vol
          _ -> pure ()

        -- Rate limit: check SDL queue depth BEFORE decoding another
        -- chunk. Once the queue exceeds 5 seconds of audio at the
        -- actual SDL device rate, we add a proportional delay so the
        -- decode loop slows to match real-time consumption, preventing
        -- unbounded queue growth.
        --
        -- The 5-second threshold and the per-128-KB-block delay are
        -- both scaled by the actual rate from aeRateVar (captured in
        -- the obtained AudioSpec at device open). If the OS audio
        -- server (e.g. WSLG PulseAudio) opens the device at a slightly
        -- different rate than 44100 Hz, the rate limiter calibrates
        -- to the real consumption rate rather than a hardcoded
        -- assumption — this stops the queue from oscillating when
        -- the actual rate is e.g. 87 % of 44100 Hz.
        --
        -- The 1 s cap on per-iteration delay is preserved: that's
        -- already enough to drain ~5 s of audio at any reasonable
        -- rate, so longer delays would just stall the decode loop.
        qBefore <- liftIO $ c_sdl_get_queued_audio_size aeDeviceId
        actualRate  <- liftIO $ STM.atomically $ STM.readTVar aeRateVar
        actualChans <- liftIO $ STM.atomically $ STM.readTVar aeChannelsVar
        let sdlBytesPerSec = fromIntegral actualRate * fromIntegral actualChans * 2
            safetyBytes    = floor (sdlBytesPerSec * 5) :: Int
        when (fromIntegral qBefore >= safetyBytes) $ do
          let rateScaleNum   = 44100 :: Int
              rateScaleDen   = max 1 actualRate
              excessBytes    = fromIntegral qBefore - safetyBytes
              excess128KB    = max 1 (excessBytes `div` 131072 + 1)
              perBlockDelayUs = (rateScaleNum * 100000) `div` rateScaleDen
              delayUs        = min 1000000 (excess128KB * perBlockDelayUs)
          traceEvent "rate_limit"
            [ "q_kb=" <> show (qBefore `div` 1024)
            , "delay_ms=" <> show (delayUs `div` 1000)
            , "actual_rate=" <> show actualRate
            , "safety_kb=" <> show (safetyBytes `div` 1024)
            ]
          liftIO $ threadDelay delayUs

        -- Read from the MP3 queue (filled concurrently by the
        -- background reader thread). The queue decouples decode from
        -- network I/O, smoothing out per-chunk timing jitter.
        let minBatch = 8192
        batch <- liftIO $ gatherFromQueue minBatch peMp3Queue peReaderDone
        if BS.null batch
          then do
            liftIO $ STM.atomically $ STM.writeTChan aeDecodedChan Nothing
            traceEvent "stream_end" []
          else do
            mAcc <- processChunk batch accBytes
            case mAcc of
              Nothing -> pure ()    -- decode error, abort
              Just newAcc -> do
                -- Pre-buffer: keep the SDL device paused until we have
                -- enough PCM queued. This ensures audio starts with a
                -- meaningful buffer (~3 s) rather than trickling in.
                -- Also log the actual SDL device rate on first
                -- unpause, so any sample-rate mismatch shows up in
                -- the trace instead of being inferred from queue
                -- oscillation.
                unless started $ do
                  when (newAcc >= peBufferTarget) $ do
                    actualRate  <- liftIO $ STM.atomically $ STM.readTVar aeRateVar
                    actualChans <- liftIO $ STM.atomically $ STM.readTVar aeChannelsVar
                    let sdlBytesPerSec = fromIntegral actualRate * fromIntegral actualChans * 2
                        safetyBytes    = floor (sdlBytesPerSec * 5) :: Int
                    traceEvent "device_start"
                      [ "queued_kb=" <> show (newAcc `div` 1024)
                      , "actual_rate=" <> show actualRate
                      , "actual_chans=" <> show actualChans
                      , "safety_kb=" <> show (safetyBytes `div` 1024)
                      ]
                    _ <- liftIO $ c_sdl_pause_audio_device aeDeviceId 0
                    pure ()

                -- Update state based on accumulated PCM bytes.
                -- (Queue depth is checked before the next gather
                -- call, not here, so the rate limiter prevents growth
                -- rather than reacting to it.)
                if newAcc < peBufferTarget
                  then liftIO $
                    STM.atomically $ STM.writeTVar aeStateVar (Buffering newAcc peBufferTarget)
                  else do
                    curVol <- liftIO $ STM.atomically $ STM.readTVar aeVolumeVar
                    liftIO $ STM.atomically $ STM.writeTVar aeStateVar (Playing curVol)
                    liftIO $ STM.atomically $ STM.writeTVar aeStreamHealth StreamGood

                let started' = started || newAcc >= peBufferTarget
                decodeLoopStream newAcc started'

-- | Block in a polling loop while the SDL device is paused. Reads
-- commands from @aeCmdChan@ directly (the commandProcessor is blocked
-- inside the decode loop and cannot dispatch them). Returns 'True'
-- if the caller should resume decoding (CmdResume received), 'False'
-- if the caller should exit (cancel token was set).
--
-- The polling interval is deliberately short (100 ms) so that
-- pause/unpause feels responsive; the thread is doing no real work
-- while paused.
handlePause_
  :: Word32                    -- ^ SDL device ID
  -> STM.TChan AudioCommand   -- ^ command channel (polled for Resume/Stop/Play)
  -> STM.TVar PlayerState     -- ^ player state TVar
  -> STM.TVar Double          -- ^ volume TVar
  -> STM.TVar Bool            -- ^ cancel token TVar
  -> STM.TVar StreamHealth    -- ^ stream health TVar
  -> IO Bool                  -- ^ True = resumed, False = cancelled/quit
handlePause_ devId cmdChan stateVar volumeVar cancelVar healthVar = do
  STM.atomically $ STM.writeTVar stateVar Paused
  let poll = do
        mbCmd <- STM.atomically $ STM.tryReadTChan cmdChan
        case mbCmd of
          Just CmdResume -> do
            when debugEnabled $
              System.IO.hPutStrLn System.IO.stderr "D pause_resume []"
            curVol <- STM.atomically $ STM.readTVar volumeVar
            _ <- c_sdl_pause_audio_device devId 0
            STM.atomically $ STM.writeTVar stateVar (Playing curVol)
            STM.atomically $ STM.writeTVar healthVar StreamGood
            pure True
          Just CmdStop -> do
            when debugEnabled $
              System.IO.hPutStrLn System.IO.stderr "D pause_stop []"
            STM.atomically $ STM.writeTVar cancelVar True
            pure False
          Just CmdQuit -> do
            when debugEnabled $
              System.IO.hPutStrLn System.IO.stderr "D pause_quit []"
            STM.atomically $ STM.writeTVar cancelVar True
            pure False
          Just CmdPlay{} -> do
            -- Do NOT consume CmdPlay from the channel: commandProcessor
            -- must see it after decodeLoopStream returns so it can
            -- call startPlayback with the new URL.  We only set the
            -- cancel token so the decode loop exits and control
            -- returns to commandProcessor.
            when debugEnabled $
              System.IO.hPutStrLn System.IO.stderr "D pause_play []"
            STM.atomically $ STM.writeTVar cancelVar True
            pure False
          Just CmdPause -> do
            -- Already paused; consume and loop
            poll
          Just (CmdVolume vol) -> do
            STM.atomically $ STM.writeTVar volumeVar (min 1.0 $ max 0.0 vol)
            poll
          Nothing -> do
            threadDelay 100000
            poll
  poll

-- | Decode and queue a single stream chunk, returning the updated byte
-- accumulator ('Nothing' on a decode error, which aborts playback).
processChunk
  :: (Reader AudioEngine :> es, Reader PlaybackEnv :> es, Tracer :> es, IOE :> es)
  => ByteString -> Int -> Eff es (Maybe Int)
processChunk chunk acc = do
  AudioEngine
    { aeStateVar    = stateVar
    , aeDecodedChan = decodedChan
    , aeVolumeVar   = volumeVar
    , aeDeviceId    = devId
    } <- ask
  PlaybackEnv{ peDecoderHandle = decoderHandle } <- ask
  t0 <- liftIO getCurrentTime
  frames <- liftIO $ Decoder.mpg123Feed decoderHandle chunk
  case frames of
    Left err -> liftIO $ do
      STM.atomically $ STM.writeTVar stateVar (ErrorOccurred (renderError err))
      STM.atomically $ STM.writeTChan decodedChan Nothing
      pure Nothing
    Right [] -> pure (Just acc)
    Right frameList -> do
      t1 <- liftIO getCurrentTime
      let frameBytes = sum (map (BS.length . afPcmData) frameList)
          newAccum   = acc + frameBytes
          nFrames    = length frameList
          decodeMs   = round (realToFrac (diffUTCTime t1 t0) * (1000 :: Double)) :: Int
          -- Log first frame's format to detect device mismatches
          (f:_)   = frameList
          fmtInfo = show (afRate f) <> "hz_" <> show (afChannels f) <> "ch"
      curVol <- liftIO $ STM.atomically $ STM.readTVar volumeVar
      t2 <- liftIO getCurrentTime
      -- Concatenate all frames' PCM into one buffer: avoids N FFI calls
      -- and N ByteString allocations from adjustVolume, reducing GC
      -- pressure and queue overhead. mconcat is a no-copy spine walk
      -- for the common case of a handful of frames (~4608 bytes each).
      let allPcm = mconcat (map afPcmData frameList)
      adjusted <- liftIO $ adjustVolume allPcm curVol
      rc <- liftIO $ queueToDevice devId adjusted
      when (rc /= 0) $ do
        sdlErr <- liftIO $ c_sdl_get_error >>= peekCString
        traceEvent "queue_err"
          [ "rc=" <> show rc
          , "sdl_err=" <> show sdlErr
          ]
      t3 <- liftIO getCurrentTime
      let adjustMs = round (realToFrac (diffUTCTime t3 t2) * (1000 :: Double)) :: Int
      traceEvent "process"
        [ "chunk_kb=" <> show (BS.length chunk `div` 1024)
        , "frames=" <> show nFrames
        , "pcm_kb=" <> show (frameBytes `div` 1024)
        , "decode_ms=" <> show decodeMs
        , "adjust_ms=" <> show adjustMs
        , "fmt=" <> fmtInfo
        ]
      pure (Just newAccum)
-------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

audioCommand :: AudioEngine -> AudioCommand -> IO ()
audioCommand engine cmd =
  STM.atomically $ STM.writeTChan (aeCmdChan engine) cmd

readPlayerState :: AudioEngine -> IO PlayerState
readPlayerState engine =
  STM.atomically $ STM.readTVar (aeStateVar engine)

readVolume :: AudioEngine -> IO Double
readVolume engine =
  STM.atomically $ STM.readTVar (aeVolumeVar engine)

readStreamHealth :: AudioEngine -> IO StreamHealth
readStreamHealth engine =
  STM.atomically $ STM.readTVar (aeStreamHealth engine)

-- | Read the most recently parsed ICY 'StreamTitle' from the
-- currently playing stream, if any. Returns 'Nothing' when the
-- stream hasn't sent a metadata block yet, or when it explicitly
-- sent an empty value. The title is reset to 'Nothing' on each
-- new 'CmdPlay' (see 'startPlayback').
readIcyMetadata :: AudioEngine -> IO (Maybe Text)
readIcyMetadata engine =
  STM.atomically $ STM.readTVar (aeIcyMetaVar engine)

closeAudio :: AudioEngine -> IO ()
closeAudio engine = do
  STM.atomically $ STM.writeTVar (aeCancelToken engine) True
  SDLRaw.closeAudioDevice (aeDeviceId engine)
  SDL.quit
