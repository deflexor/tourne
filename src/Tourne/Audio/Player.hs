module Tourne.Audio.Player
  ( AudioEngine
  , initAudio
  , closeAudio
  , audioCommand
  , readPlayerState
  , readVolume
  , readStreamHealth
  , AudioCommand (..)
  ) where

import Relude hiding (Reader, runReader, ask, asks, local, MonadReader)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM qualified as STM
import Data.ByteString qualified as BS
import Data.IORef qualified as IORef
import Foreign.Ptr (nullFunPtr, nullPtr)
import Foreign.Storable (poke, peek)
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.Types (CInt)
import Foreign.C.String (peekCString)
import Network.HTTP.Client (Manager)
import System.Directory (doesPathExist)
import System.Environment qualified as Env
import System.IO qualified
import Data.Text qualified
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Char (toLower)
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
import Tourne.Audio.Stream.Shim qualified as Stream.Shim
import Tourne.Effect.Tracer (Tracer, runTracer, traceEvent)
import Tourne.Effect.HttpClient (HttpClient, runHttpClient, getManager)

import Tourne.Audio.Player.State
  ( AudioEngine (..), PlaybackEnv (..), queueSafetyBytes )
import Tourne.Audio.Player.FFI
  ( c_sdl_get_queued_audio_size, c_sdl_get_audio_device_status
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
    { aeStateVar     = stateVar
    , aeStreamHealth = healthVar
    , aeDeviceId     = devId
    , aeCancelToken  = cancelVar
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
  streamResult <- liftIO $ Stream.openStream' httpMgr url streamTrace
  case streamResult of
    Left err -> liftIO $ do
      STM.atomically $ STM.writeTVar stateVar (ErrorOccurred (renderError err))
      STM.atomically $ STM.writeTVar healthVar (StreamLost (renderError err))

    Right stream -> do
      -- Create legacy handle via shim for backward compat.
      streamHandle <- liftIO $ Stream.Shim.streamToHandleWithUrl stream url

      decoderResult <- liftIO Decoder.mpg123Open
      case decoderResult of
        Left err -> liftIO $ do
          STM.atomically $ STM.writeTVar stateVar (ErrorOccurred (renderError err))
          Stream.closeStream streamHandle

        Right decoderHandle -> do
          -- Pre-buffer target: 512 KB (~3 seconds at 44100/16/2).
          -- A larger value made the user wait too long before
          -- audio started, especially on connections where the
          -- radio server throttles to 1-2x the encoded bitrate.
          let bufferTargetBytes = 524288

          liftIO $ STM.atomically $ STM.writeTVar stateVar (Buffering 0 bufferTargetBytes)
          liftIO $ STM.atomically $ STM.writeTVar healthVar StreamGood

          -- Drop any PCM bytes still queued from a previous
          -- playback. Without this, the new stream's first samples
          -- are appended after the old stream's tail, producing
          -- audible bleed-through and timing glitches at the
          -- switchover. Clearing while the device is paused (next
          -- step) is the documented safe way per the SDL2 API.
          liftIO $ c_sdl_clear_queued_audio devId

          -- Start/resume playback (0 = unpause). Check return value.
          pauseRc <- liftIO $ c_sdl_pause_audio_device devId 0
          devStatus <- liftIO $ c_sdl_get_audio_device_status devId
          initQueue <- liftIO $ c_sdl_get_queued_audio_size devId
          when (pauseRc /= 0) $
            traceEvent "pause_err" ["rc=" <> show pauseRc, "devId=" <> show devId]
          traceEvent "play_start"
            [ "dev_status=" <> show devStatus
            , "queue_size=" <> show initQueue
            ]

          -- Per-playback environment: stream + decoder handles.
          let pbEnv = PlaybackEnv
                { peStreamHandle  = streamHandle
                , peStream        = stream
                , peDecoderHandle = decoderHandle
                , peBufferTarget  = bufferTargetBytes
                }

          -- Run the decode loop with the per-playback env in scope, then clean up.
          runReader pbEnv (decodeLoop 0)

          liftIO $ Decoder.mpg123Close decoderHandle
          liftIO $ Stream.closeStream streamHandle
          liftIO $ SDLRaw.pauseAudioDevice devId 1

--------------------------------------------------------------------------------
-- Decode loop and chunk processing
--------------------------------------------------------------------------------

-- | Main decode loop: read stream -> decode -> queue to SDL2. Uses
-- non-blocking stream reads when the SDL queue is healthy, falling
-- back to blocking reads only when the queue runs low. This
-- prevents freezing on network stalls while keeping the device fed.
decodeLoop
  :: (Reader AudioEngine :> es, Reader PlaybackEnv :> es, Tracer :> es, IOE :> es)
  => Int  -- ^ accumulated decoded bytes so far
  -> Eff es ()
decodeLoop accBytes = do
  AudioEngine
    { aeCancelToken  = cancelVar
    , aeCmdChan      = cmdChan
    , aeStateVar     = stateVar
    , aeStreamHealth = healthVar
    , aeVolumeVar    = volumeVar
    , aeDecodedChan  = decodedChan
    , aeDeviceId     = devId
    } <- ask
  PlaybackEnv
    { peStreamHandle = streamHandle
    , peBufferTarget = bufferTargetBytes
    } <- ask

  cancelled <- liftIO $ STM.atomically $ STM.readTVar cancelVar
  unless cancelled $ do
    peekedCmd <- liftIO $ STM.atomically $ STM.tryPeekTChan cmdChan
    case peekedCmd of
      Just CmdStop -> do
        traceEvent "decode_peek" ["cmd=CmdStop"]
        liftIO $ void $ STM.atomically $ STM.tryReadTChan cmdChan
        -- Consume the cmd and signal the cmdThread to stop the
        -- current playback. The cmdProcessor's startPlayback will
        -- return, the loop will recurse, and CmdStop will be
        -- processed (which is a no-op now since we already did it).
        liftIO $ STM.atomically $ STM.writeTVar cancelVar True
      Just CmdQuit -> do
        traceEvent "decode_peek" ["cmd=CmdQuit"]
        liftIO $ void $ STM.atomically $ STM.tryReadTChan cmdChan
        liftIO $ STM.atomically $ STM.writeTVar cancelVar True
      Just CmdPlay{} -> do
        -- New station selected. The cmdThread is busy inside
        -- startPlayback (us), so it cannot process this new CmdPlay
        -- itself. We self-interrupt: signal cancel, exit the loop,
        -- let the cmdThread see cancel on the next iteration, and
        -- then re-enter the loop in commandProcessor where it
        -- picks up this CmdPlay from the channel.
        traceEvent "decode_peek" ["cmd=CmdPlay"]
        liftIO $ STM.atomically $ STM.writeTVar cancelVar True
      _ -> do
        -- Volume commands are applied inline without breaking playback.
        case peekedCmd of
          Just (CmdVolume vol) -> do
            liftIO $ void $ STM.atomically $ STM.tryReadTChan cmdChan
            liftIO $ STM.atomically $ STM.writeTVar volumeVar vol
          _ -> pure ()

        extraChunks <- liftIO $ Stream.drainStreamChunks streamHandle
        case extraChunks of
          (firstChunk:moreChunks) -> do
            mResult <- processChunks firstChunk moreChunks accBytes
            case mResult of
              Nothing -> pure ()
              Just (acc', _, devStatus) ->
                if devStatus == 0
                  then pure ()
                  else if acc' < bufferTargetBytes
                    then do
                      liftIO $ STM.atomically $ STM.writeTVar stateVar (Buffering acc' bufferTargetBytes)
                      decodeLoop acc'
                    else do
                      curVol <- liftIO $ STM.atomically $ STM.readTVar volumeVar
                      liftIO $ STM.atomically $ STM.writeTVar stateVar (Playing curVol)
                      liftIO $ STM.atomically $ STM.writeTVar healthVar StreamGood
                      decodeLoop acc'

          [] -> do
            qNow <- liftIO $ c_sdl_get_queued_audio_size devId
            devStatus <- liftIO $ c_sdl_get_audio_device_status devId
            savedSdlErr <- liftIO $ if devStatus == 0
              then fmap Just (c_sdl_get_error >>= peekCString)
              else pure Nothing
            let qNowKb = fromIntegral qNow `div` 1024 :: Int
            case savedSdlErr of
              Just err -> traceEvent "dev_stopped" ["sdl_err=" <> show err, "queue_kb=" <> show qNowKb]
              Nothing -> traceEvent "idle" ["queue_kb=" <> show qNowKb, "status=" <> show devStatus]

            when (devStatus /= 0) $
              if qNow < queueSafetyBytes && accBytes > 0
                then do
                  -- Queue running low (not first iteration): block for data.
                  mbChunk <- liftIO $ Stream.readStreamChunk streamHandle
                  case mbChunk of
                    Nothing -> do
                      liftIO $ STM.atomically $ STM.writeTChan decodedChan Nothing
                      traceEvent "stream_end" []
                      -- Stream is over; exit the decode loop so we
                      -- don't busy-poll 'queue healthy: sleep 100ms'
                      -- forever. The audio callback will see
                      -- 'Nothing' on 'decodedChan' and stop on its
                      -- own. Without this 'return', the loop falls
                      -- through into the 'else' branch and spins at
                      -- 100ms indefinitely.
                      pure ()
                    Just firstChunk -> do
                      extra <- liftIO $ Stream.drainStreamChunks streamHandle
                      mResult <- processChunks firstChunk extra accBytes
                      case mResult of
                        Nothing -> pure ()
                        Just (acc', _, devStat) ->
                          if devStat == 0
                            then pure ()
                            else do
                              if acc' < bufferTargetBytes
                                then liftIO $
                                  STM.atomically $ STM.writeTVar stateVar (Buffering acc' bufferTargetBytes)
                                else do
                                  curVol <- liftIO $ STM.atomically $ STM.readTVar volumeVar
                                  liftIO $ STM.atomically $ STM.writeTVar stateVar (Playing curVol)
                                  liftIO $ STM.atomically $ STM.writeTVar healthVar StreamGood
                              decodeLoop acc'
                else do
                  -- Queue healthy: sleep briefly then retry.
                  liftIO $ threadDelay 100000  -- 100ms
                  decodeLoop accBytes

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
          fmtInfo = case frameList of
            []     -> ""
            (f:_) -> show (afRate f) <> "hz_" <> show (afChannels f) <> "ch"
      curVol <- liftIO $ STM.atomically $ STM.readTVar volumeVar
      t2 <- liftIO getCurrentTime
      forM_ frameList $ \frame -> do
        adjusted <- liftIO $ adjustVolume (afPcmData frame) curVol
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

-- | Process the first chunk plus any additionally-drained chunks, logging the
-- read/cycle summary. Returns the updated accumulator, queue size, and device
-- status ('Nothing' if a decode error aborted the batch).
processChunks
  :: (Reader AudioEngine :> es, Reader PlaybackEnv :> es, Tracer :> es, IOE :> es)
  => ByteString -> [ByteString] -> Int -> Eff es (Maybe (Int, Word32, CInt))
processChunks firstChunk extraChunks accBytes = do
  AudioEngine{ aeDeviceId = devId } <- ask
  PlaybackEnv{ peBufferTarget = bufferTargetBytes } <- ask
  qBefore <- liftIO $ c_sdl_get_queued_audio_size devId
  traceEvent "read"
    [ "chunk_kb=" <> show (BS.length firstChunk `div` 1024)
    , "queue_before_kb=" <> show (fromIntegral qBefore `div` 1024 :: Int)
    , "n_chunks=" <> show (1 + length extraChunks)
    ]
  mAcc <- processChunk firstChunk accBytes
  case mAcc of
    Nothing -> pure Nothing
    Just newAcc -> do
      finalAcc <- drainChunks (Just newAcc) extraChunks
      case finalAcc of
        Nothing -> pure Nothing
        Just acc' -> do
          devStatus <- liftIO $ c_sdl_get_audio_device_status devId
          savedSdlErr <- liftIO $ if devStatus == 0
            then fmap Just (c_sdl_get_error >>= peekCString)
            else pure Nothing
          qAfter <- liftIO $ c_sdl_get_queued_audio_size devId
          let qAfterKb = fromIntegral qAfter `div` 1024 :: Int
              phase = if acc' < bufferTargetBytes then "buffer" else "play"
          case savedSdlErr of
            Just err -> traceEvent "dev_stopped" ["sdl_err=" <> show err, "queue_kb=" <> show qAfterKb]
            Nothing -> pure ()
          traceEvent "cycle_end"
            [ "phase=" <> phase
            , "queue_kb=" <> show qAfterKb
            , "n_extra=" <> show (length extraChunks)
            , "accum_kb=" <> show (acc' `div` 1024)
            , "status=" <> show devStatus
            ]
          pure (Just (acc', qAfter, devStatus))

-- | Drain a list of leftover chunks through 'processChunk', short-circuiting
-- on the first decode error.
drainChunks
  :: (Reader AudioEngine :> es, Reader PlaybackEnv :> es, Tracer :> es, IOE :> es)
  => Maybe Int -> [ByteString] -> Eff es (Maybe Int)
drainChunks mAcc' chunks = case chunks of
  []     -> pure mAcc'
  (c:cs) -> do
    m <- case mAcc' of
      Nothing -> pure Nothing
      Just a  -> processChunk c a
    drainChunks m cs

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

closeAudio :: AudioEngine -> IO ()
closeAudio engine = do
  STM.atomically $ STM.writeTVar (aeCancelToken engine) True
  SDLRaw.closeAudioDevice (aeDeviceId engine)
  SDL.quit
