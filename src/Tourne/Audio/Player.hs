module Tourne.Audio.Player
  ( AudioEngine
  , initAudio
  , closeAudio
  , audioCommand
  , readPlayerState
  , readVolume
  , readStreamHealth
  , AudioCommand(..)
  ) where

import Relude hiding (hFlush, Reader, runReader, ask, asks, local)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM qualified as STM
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.ByteString.Internal (unsafeCreate)
import Data.IORef qualified as IORef
import Foreign.Ptr (Ptr, castPtr, nullFunPtr, nullPtr)
import Foreign.Storable (poke, peek, peekByteOff, pokeByteOff)
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.Types (CDouble(..), CInt(..))
import Foreign.C.String (CString, peekCString)
import System.Directory (doesPathExist)
import System.Environment qualified as Env
import System.IO (hFlush)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import Data.Text qualified as Text
import Data.Char (toLower)
import Effectful
import Effectful.Reader.Static
import SDL qualified
import SDL.Raw.Audio qualified as SDLRaw
import SDL.Raw.Types (AudioSpec(..))


import Tourne.Types
import Tourne.Error (AppError (..), renderError)
import Tourne.Audio.Types
import Tourne.Audio.Decoder qualified as Decoder
import Tourne.Audio.Stream qualified as Stream
import Tourne.Effect.Tracer (Tracer, runTracer, traceEvent)

--------------------------------------------------------------------------------
-- FFI imports
--------------------------------------------------------------------------------

-- | FFI import for SDL_GetQueuedAudioSize (not exposed by Haskell sdl2 bindings)
foreign import ccall "SDL_GetQueuedAudioSize"
  c_sdl_get_queued_audio_size :: Word32 -> IO Word32

-- | FFI import for SDL_GetAudioDeviceStatus
-- Returns: 0=STOPPED, 1=PLAYING, 2=PAUSED
foreign import ccall "SDL_GetAudioDeviceStatus"
  c_sdl_get_audio_device_status :: Word32 -> IO CInt

-- | FFI import for SDL_PauseAudioDevice (with return value)
-- Returns 0 on success, -1 on error
foreign import ccall "SDL_PauseAudioDevice"
  c_sdl_pause_audio_device :: Word32 -> CInt -> IO CInt

-- | FFI import for SDL_GetError
-- Returns a string describing the last SDL error
foreign import ccall "SDL_GetError"
  c_sdl_get_error :: IO CString

--------------------------------------------------------------------------------
-- Audio engine handle
--------------------------------------------------------------------------------

data AudioEngine = AudioEngine
  { aeDecodedChan   :: !(STM.TChan (Maybe ByteString))
  , aeCmdChan       :: !(STM.TChan AudioCommand)
  , aeStateVar      :: !(STM.TVar PlayerState)
  , aeStreamHealth  :: !(STM.TVar StreamHealth)
  , aeVolumeVar     :: !(STM.TVar Double)
  , aeRateVar       :: !(STM.TVar Int)
  , aeChannelsVar   :: !(STM.TVar Int)
  , aeDeviceId      :: !Word32  -- SDL2 AudioDeviceID
  , aeCancelToken   :: !(STM.TVar Bool)
  , aeLeftoverVar   :: !(IORef.IORef ByteString)
  } deriving (Generic)

--------------------------------------------------------------------------------
-- Per-playback environment (created fresh for each CmdPlay)
--------------------------------------------------------------------------------

-- | State local to a single playback run: the stream + decoder handles, the
-- debug timer, and the buffering target. Distinct from 'AudioEngine' (shared,
-- long-lived) because each 'CmdPlay' opens its own stream and decoder.
data PlaybackEnv = PlaybackEnv
  { peStreamHandle  :: !Stream.StreamHandle
  , peDecoderHandle :: !(Ptr Decoder.Mpg123Handle)
  , peBufferTarget  :: !Int
  }

-- | Minimum SDL queue size before the decode loop falls back to a blocking
-- stream read. Below this, the loop blocks on the network to keep the device
-- fed; above it, it spins (non-blocking) to avoid freezing on stalls.
queueSafetyBytes :: Word32
queueSafetyBytes = 262144  -- 256KB

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- | Initialize audio system
initAudio :: IO (Either AppError AudioEngine)
initAudio = do
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
        runEff $ runTracer tracerStart tracerEnabled $
        runReader engine commandProcessor

      pure (Right engine)

--------------------------------------------------------------------------------
-- Command processing
--------------------------------------------------------------------------------

-- | Background command loop. Reads 'AudioCommand's from the command channel
-- and dispatches them, running 'startPlayback' for 'CmdPlay'. All shared
-- state lives in the 'AudioEngine' environment (no parameter threading).
commandProcessor :: (Reader AudioEngine :> es, Tracer :> es, IOE :> es) => Eff es ()
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
        cancel <- liftIO $ STM.atomically $ STM.readTVar cancelVar
        unless cancel $ do
          cmd <- liftIO $ STM.atomically $ STM.readTChan cmdChan
          case cmd of
            CmdPlay url -> do
              liftIO $ STM.atomically $ STM.writeTVar stateVar (Connecting url)
              startPlayback url
              liftIO $ STM.atomically $ STM.writeTVar stateVar Stopped

            CmdStop -> do
              liftIO $ STM.atomically $ STM.writeTVar stateVar Stopped
              liftIO $ drainChan decodedChan

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
  :: (Reader AudioEngine :> es, Tracer :> es, IOE :> es)
  => Text
  -> Eff es ()
startPlayback url = do
  AudioEngine
    { aeStateVar     = stateVar
    , aeStreamHealth = healthVar
    , aeDeviceId     = devId
    } <- ask
  -- Extract a Tracer -> IO callback so Stream.openStream (a pure
  -- IO function called from inside an Eff context) can route its
  -- debug events through the same Tracer interpreter as the decode
  -- loop. The withRunInIO unlift captures the Eff's environment.
  streamTrace <- withRunInIO $ \runInIO -> pure (\l fs -> runInIO (traceEvent l fs))
  streamResult <- liftIO $ Stream.openStream url streamTrace
  case streamResult of
    Left err -> liftIO $ do
      STM.atomically $ STM.writeTVar stateVar (ErrorOccurred (renderError err))
      STM.atomically $ STM.writeTVar healthVar (StreamLost (renderError err))

    Right streamHandle -> do
      decoderResult <- liftIO Decoder.mpg123Open
      case decoderResult of
        Left err -> liftIO $ do
          STM.atomically $ STM.writeTVar stateVar (ErrorOccurred (renderError err))
          Stream.closeStream streamHandle

        Right decoderHandle -> do
          -- Pre-buffer target: 2 MB (~12 seconds at 44100/16/2).
          let bufferTargetBytes = 2097152

          liftIO $ STM.atomically $ STM.writeTVar stateVar (Buffering 0 bufferTargetBytes)
          liftIO $ STM.atomically $ STM.writeTVar healthVar StreamGood

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
                , peDecoderHandle = decoderHandle
                , peBufferTarget  = bufferTargetBytes
                }

          -- Run the decode loop with the per-playback env in scope, then clean up.
          runReader pbEnv (decodeLoop 0)

          liftIO $ Decoder.mpg123Close decoderHandle
          liftIO $ Stream.closeStream streamHandle
          liftIO $ SDLRaw.pauseAudioDevice devId 1

--------------------------------------------------------------------------------
-- Decode loop and chunk processing (extracted top-level, individually testable)
--------------------------------------------------------------------------------

-- | Main decode loop: read stream -> decode -> queue to SDL2. Uses
-- non-blocking stream reads when the SDL queue is healthy, falling back to
-- blocking reads only when the queue runs low. This prevents freezing on
-- network stalls while keeping the device fed.
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
      Just CmdQuit -> do
        traceEvent "decode_peek" ["cmd=CmdQuit"]
        liftIO $ void $ STM.atomically $ STM.tryReadTChan cmdChan
      Just CmdPlay{} -> traceEvent "decode_peek" ["cmd=CmdPlay"]
      _ -> do
        -- Volume commands are applied inline without breaking playback.
        case peekedCmd of
          Just (CmdVolume vol) -> liftIO $ do
            void $ STM.atomically $ STM.tryReadTChan cmdChan
            STM.atomically $ STM.writeTVar volumeVar vol
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

-- | Queue PCM data to SDL2 audio device using raw queueAudio.
-- Returns 0 on success, -1 on error (SDL_GetError for details).
queueToDevice :: Word32 -> ByteString -> IO CInt
queueToDevice devId bs = do
  let bufSize = fromIntegral (BS.length bs) :: Word32
  BS.useAsCString bs $ \cstr ->
    SDLRaw.queueAudio devId (castPtr cstr) bufSize

-- | Apply volume to PCM 16-bit signed little-endian samples.
-- Uses pointer-based processing (unsafeCreate) to avoid list allocation
-- and GC pressure that caused audio stuttering.
--
-- Lives in 'IO' because the body calls 'unsafeCreate'; the early-return
-- branches are constant-time and allocation-free.
adjustVolume :: ByteString -> Double -> IO ByteString
adjustVolume bs volFactor
  | volFactor >= 1.0 = pure bs
  | volFactor <= 0.0 = pure (BS.replicate (BS.length bs) 0)
  | otherwise = unsafeUseAsCString bs $ \src ->
      let len     = BS.length bs
          halfLen = len `div` 2
          volD    = CDouble volFactor
      in pure (unsafeCreate len $ \dst -> do
        let go i
              | i >= halfLen = pure ()
              | otherwise = do
                  w <- peekByteOff src (i * 2) :: IO Word16
                  let unsigned = fromIntegral w :: Int
                      signed   = if unsigned >= 0x8000
                                   then unsigned - 0x10000
                                   else unsigned
                      adjusted = fromIntegral
                        (round (fromIntegral signed * volD) :: Int) :: Word16
                  pokeByteOff dst (i * 2) adjusted
                  go (i + 1)
        go 0)

-- | Drain all remaining items from a channel
drainChan :: STM.TChan a -> IO ()
drainChan chan = STM.atomically $ go
  where
    go = do
      mb <- STM.tryReadTChan chan
      case mb of
        Nothing -> pure ()
        Just _  -> go

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
