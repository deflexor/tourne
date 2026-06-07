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

import Relude hiding (hFlush)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM qualified as STM
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.ByteString.Internal (unsafeCreate)
import Data.IORef qualified as IORef
import Foreign.Ptr (castPtr, nullFunPtr, nullPtr)
import Foreign.Storable (poke, peek, peekByteOff, pokeByteOff)
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.Types (CDouble(..), CInt(..))
import Foreign.C.String (CString, peekCString)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (doesPathExist)
import System.Environment qualified as Env
import System.IO (hFlush, hPutStrLn)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import Data.Text qualified as Text
import SDL qualified
import SDL.Raw.Audio qualified as SDLRaw
import SDL.Raw.Types (AudioSpec(..))


import Tourne.Types
import Tourne.Audio.Types
import Tourne.Audio.Decoder qualified as Decoder
import Tourne.Audio.Stream qualified as Stream

--------------------------------------------------------------------------------
-- Debug logging
--------------------------------------------------------------------------------

-- | Set to True to enable per-iteration debug logging to stderr.
debugEnabled :: Bool
debugEnabled = True

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

-- | Write a timestamped debug line to stderr. Format:
--   D <elapsed_ms> <label> <key=val> <key=val> ...
debugLog :: IORef.IORef UTCTime -> Text -> [Text] -> IO ()
debugLog startRef label fields = when debugEnabled $ do
  start <- IORef.readIORef startRef
  now <- getCurrentTime
  let elapsed = round (realToFrac (diffUTCTime now start) * (1000 :: Double)) :: Int
      line = "D " <> show elapsed <> " " <> toString label
           <> " " <> toString (Text.intercalate " " fields)
  hPutStrLn stderr line
  hFlush stderr

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
-- Initialization
--------------------------------------------------------------------------------

-- | Initialize audio system
initAudio :: IO AudioEngine
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
  (devId, actualRate, actualChans) <- alloca $ \desired ->
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
      when (rawDevId == 0) $
        error "Failed to open audio device (queue mode)"
      -- Read actual audio spec from SDL
      spec <- peek obtained
      let freq = fromIntegral (audioSpecFreq spec) :: Int
          chans = fromIntegral (audioSpecChannels spec) :: Int
      pure (rawDevId, freq, chans)

  -- Store actual device rate in the rate/channel TVars
  STM.atomically $ STM.writeTVar rateVar actualRate
  STM.atomically $ STM.writeTVar chansVar actualChans

  -- Start processing commands in background
  _cmdThread <- async $ commandProcessor cmdChan decodedChan stateVar healthVar
                               rateVar chansVar cancelVar volumeVar devId

  pure AudioEngine
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

--------------------------------------------------------------------------------
-- Command processing
--------------------------------------------------------------------------------

commandProcessor
  :: STM.TChan AudioCommand
  -> STM.TChan (Maybe ByteString)
  -> STM.TVar PlayerState
  -> STM.TVar StreamHealth
  -> STM.TVar Int
  -> STM.TVar Int
  -> STM.TVar Bool
  -> STM.TVar Double
  -> Word32
  -> IO ()
commandProcessor cmdChan decodedChan stateVar healthVar
                  rateVar chansVar cancelVar volumeVar devId = do
    let loop = do
          cancel <- STM.atomically $ STM.readTVar cancelVar
          when (not cancel) $ do
            cmd <- STM.atomically $ STM.readTChan cmdChan
            case cmd of
              CmdPlay url -> do
                STM.atomically $ STM.writeTVar stateVar (Connecting url)
                startPlayback url decodedChan stateVar healthVar rateVar
                             chansVar cancelVar volumeVar devId cmdChan
                STM.atomically $ STM.writeTVar stateVar Stopped

              CmdStop -> do
                STM.atomically $ STM.writeTVar stateVar Stopped
                drainChan decodedChan

              CmdPause -> do
                SDLRaw.pauseAudioDevice devId 1
                STM.atomically $ STM.writeTVar stateVar Paused

              CmdResume -> do
                curVol <- STM.atomically $ STM.readTVar volumeVar
                SDLRaw.pauseAudioDevice devId 0
                STM.atomically $ STM.writeTVar stateVar (Playing curVol)

              CmdVolume vol -> do
                STM.atomically $ STM.writeTVar volumeVar (min 1.0 $ max 0.0 vol)

              CmdQuit -> do
                STM.atomically $ STM.writeTVar cancelVar True
                drainChan decodedChan

            loop
    loop

-- | Start actual playback of a stream URL using queued audio
startPlayback
  :: Text
  -> STM.TChan (Maybe ByteString)
  -> STM.TVar PlayerState
  -> STM.TVar StreamHealth
  -> STM.TVar Int
  -> STM.TVar Int
  -> STM.TVar Bool
  -> STM.TVar Double
  -> Word32
  -> STM.TChan AudioCommand
  -> IO ()
startPlayback url decodedChan stateVar healthVar
              _rateVar _chansVar cancelVar volumeVar devId cmdChan = do
  streamResult <- Stream.openStream url
  case streamResult of
    Left err -> do
      STM.atomically $ STM.writeTVar stateVar (ErrorOccurred $ toText err)
      STM.atomically $ STM.writeTVar healthVar (StreamLost $ toText err)

    Right streamHandle -> do
      decoderResult <- Decoder.mpg123Open
      case decoderResult of
        Left err -> do
          STM.atomically $ STM.writeTVar stateVar (ErrorOccurred err)
          Stream.closeStream streamHandle

        Right decoderHandle -> do
          -- Pre-buffer: 512 KB of decoded PCM before starting playback.
          -- ~3 seconds at 44100Hz 16-bit stereo, more at lower rates.
          let bufferTargetBytes = 524288  -- 512 KB

          STM.atomically $ STM.writeTVar stateVar (Buffering 0 bufferTargetBytes)
          STM.atomically $ STM.writeTVar healthVar StreamGood

          -- Timer for debug logging (created before any debugLog calls)
          startTimeRef <- IORef.newIORef =<< getCurrentTime

          -- Start/resume playback (0 = unpause). Check return value.
          pauseRc <- c_sdl_pause_audio_device devId 0
          devStatus <- c_sdl_get_audio_device_status devId
          initQueue <- c_sdl_get_queued_audio_size devId
          when (pauseRc /= 0) $
            debugLog startTimeRef "pause_err"
              [ "rc=" <> show pauseRc, "devId=" <> show devId ]
          debugLog startTimeRef "play_start"
            [ "dev_status=" <> show devStatus
            , "queue_size=" <> show initQueue
            ]

          -- Decode & queue a single chunk
          let processChunk chunk acc = do
                t0 <- getCurrentTime
                frames <- Decoder.mpg123Feed decoderHandle chunk
                case frames of
                  Left err -> do
                    STM.atomically $ STM.writeTVar stateVar (ErrorOccurred err)
                    STM.atomically $ STM.writeTChan decodedChan Nothing
                    pure Nothing
                  Right [] -> pure (Just acc)
                  Right frameList -> do
                    t1 <- getCurrentTime
                    let frameBytes = sum (map (BS.length . afPcmData) frameList)
                        newAccum   = acc + frameBytes
                        nFrames    = length frameList
                        decodeMs   = round (realToFrac (diffUTCTime t1 t0) * (1000 :: Double)) :: Int
                        -- Log first frame's format to detect device mismatches
                        fmtInfo = case frameList of
                          [] -> ""
                          (f:_) -> show (afRate f) <> "hz_" <> show (afChannels f) <> "ch"
                    curVol <- STM.atomically $ STM.readTVar volumeVar
                    t2 <- getCurrentTime
                    forM_ frameList $ \frame -> do
                      let adjusted = adjustVolume (afPcmData frame) curVol
                      rc <- queueToDevice devId adjusted
                      when (rc /= 0 && debugEnabled) $ do
                        sdlErr <- c_sdl_get_error >>= peekCString
                        debugLog startTimeRef "queue_err"
                          [ "rc=" <> show rc
                          , "sdl_err=" <> show sdlErr
                          ]
                    t3 <- getCurrentTime
                    let adjustMs = round (realToFrac (diffUTCTime t3 t2) * (1000 :: Double)) :: Int
                    debugLog startTimeRef "process"
                      [ "chunk_kb=" <> show (BS.length chunk `div` 1024)
                      , "frames=" <> show nFrames
                      , "pcm_kb=" <> show (frameBytes `div` 1024)
                      , "decode_ms=" <> show decodeMs
                      , "adjust_ms=" <> show adjustMs
                      , "fmt=" <> fmtInfo
                      ]
                    pure (Just newAccum)

          -- Decode loop: read stream → decode → queue to SDL2.
          -- Uses non-blocking stream reads when SDL queue is healthy, falling back
          -- to blocking reads only when the queue runs low. This prevents the decode
          -- loop from freezing during network stalls, keeping the SDL queue fed.
          let queueSafetyBytes = 262144  -- 256KB: minimum before falling back to blocking read

          let drainOnce mAcc' chunks =
                case chunks of
                  [] -> pure mAcc'
                  (c:cs) -> do
                    m <- case mAcc' of
                      Nothing -> pure Nothing
                      Just a  -> processChunk c a
                    drainOnce m cs

          let processChunks firstChunk extraChunks accBytes = do
                qBefore <- c_sdl_get_queued_audio_size devId
                debugLog startTimeRef "read"
                  [ "chunk_kb=" <> show (BS.length firstChunk `div` 1024)
                  , "queue_before_kb=" <> show (fromIntegral qBefore `div` 1024 :: Int)
                  , "n_chunks=" <> show (1 + length extraChunks)
                  ]
                mAcc <- processChunk firstChunk accBytes
                case mAcc of
                  Nothing -> pure Nothing
                  Just newAcc -> do
                    finalAcc <- drainOnce (Just newAcc) extraChunks
                    case finalAcc of
                      Nothing -> pure Nothing
                      Just acc' -> do
                        devStatus <- c_sdl_get_audio_device_status devId
                        savedSdlErr <- if devStatus == 0
                          then fmap Just (c_sdl_get_error >>= peekCString)
                          else pure Nothing
                        qAfter <- c_sdl_get_queued_audio_size devId
                        let qAfterKb = fromIntegral qAfter `div` 1024 :: Int
                            phase = if acc' < bufferTargetBytes then "buffer" else "play"
                        case savedSdlErr of
                          Just err ->
                            debugLog startTimeRef "dev_stopped"
                              [ "sdl_err=" <> show err, "queue_kb=" <> show qAfterKb ]
                          Nothing -> pure ()
                        debugLog startTimeRef "cycle_end"
                          [ "phase=" <> phase
                          , "queue_kb=" <> show qAfterKb
                          , "n_extra=" <> show (length extraChunks)
                          , "accum_kb=" <> show (acc' `div` 1024)
                          , "status=" <> show devStatus
                          ]
                        pure (Just (acc', qAfter, devStatus))

          let decodeLoop accBytes = do
                cancelled <- STM.atomically $ STM.readTVar cancelVar
                when (not cancelled) $ do
                  peekedCmd <- STM.atomically $ STM.tryPeekTChan cmdChan
                  case peekedCmd of
                    Just CmdStop -> do
                      debugLog startTimeRef "decode_peek" ["cmd=CmdStop"]
                      void $ STM.atomically $ STM.tryReadTChan cmdChan
                      pure ()
                    Just CmdQuit -> do
                      debugLog startTimeRef "decode_peek" ["cmd=CmdQuit"]
                      void $ STM.atomically $ STM.tryReadTChan cmdChan
                      pure ()
                    Just CmdPlay{} -> do
                      debugLog startTimeRef "decode_peek" ["cmd=CmdPlay"]
                      pure ()
                    _ -> do
                      case peekedCmd of
                        Just (CmdVolume vol) -> do
                          void $ STM.atomically $ STM.tryReadTChan cmdChan
                          STM.atomically $ STM.writeTVar volumeVar vol
                        _ -> pure ()

                      -- Try non-blocking read first; fall back to blocking if needed
                      extraChunks <- Stream.drainStreamChunks streamHandle
                      case extraChunks of
                        (firstChunk:moreChunks) -> do
                          -- Data available immediately: process all chunks
                          mResult <- processChunks firstChunk moreChunks accBytes
                          case mResult of
                            Nothing -> pure ()
                            Just (acc', qAfter, devStatus) -> do
                              if devStatus == 0
                                then pure ()
                                else if acc' < bufferTargetBytes
                                  then do
                                    STM.atomically $ STM.writeTVar stateVar (Buffering acc' bufferTargetBytes)
                                    decodeLoop acc'
                                  else do
                                    curVol <- STM.atomically $ STM.readTVar volumeVar
                                    STM.atomically $ STM.writeTVar stateVar (Playing curVol)
                                    STM.atomically $ STM.writeTVar healthVar StreamGood
                                    decodeLoop acc'

                        [] -> do
                          -- No data buffered. Check queue health.
                          qNow <- c_sdl_get_queued_audio_size devId
                          devStatus <- c_sdl_get_audio_device_status devId
                          savedSdlErr <- if devStatus == 0
                            then fmap Just (c_sdl_get_error >>= peekCString)
                            else pure Nothing
                          let qNowKb = fromIntegral qNow `div` 1024 :: Int

                          case savedSdlErr of
                            Just err -> do
                              debugLog startTimeRef "dev_stopped"
                                [ "sdl_err=" <> show err, "queue_kb=" <> show qNowKb ]
                              pure ()
                            Nothing ->
                              debugLog startTimeRef "idle"
                                [ "queue_kb=" <> show qNowKb, "status=" <> show devStatus ]

                          when (devStatus /= 0) $ do
                            -- Device still playing. Decide: poll or block.
                            if qNow < queueSafetyBytes && accBytes > 0
                              then do
                                -- Queue running low (not first iteration): block for data
                                mbChunk <- Stream.readStreamChunk streamHandle
                                case mbChunk of
                                  Nothing -> do
                                    STM.atomically $ STM.writeTChan decodedChan Nothing
                                    debugLog startTimeRef "stream_end" []
                                    pure ()
                                  Just firstChunk -> do
                                    extra <- Stream.drainStreamChunks streamHandle
                                    mResult <- processChunks firstChunk extra accBytes
                                    case mResult of
                                      Nothing -> pure ()
                                      Just (acc', _, devStat) ->
                                        if devStat == 0
                                          then pure ()
                                          else do
                                            let continue' = do
                                                  curVol <- STM.atomically $ STM.readTVar volumeVar
                                                  STM.atomically $ STM.writeTVar stateVar (Playing curVol)
                                                  STM.atomically $ STM.writeTVar healthVar StreamGood
                                                  decodeLoop acc'
                                            if acc' < bufferTargetBytes
                                              then do
                                                STM.atomically $ STM.writeTVar stateVar (Buffering acc' bufferTargetBytes)
                                                decodeLoop acc'
                                              else continue'
                              else do
                                -- Queue healthy: sleep briefly then retry
                                threadDelay 100000  -- 100ms
                                decodeLoop accBytes

          decodeLoop 0

          -- Cleanup
          Decoder.mpg123Close decoderHandle
          Stream.closeStream streamHandle
          SDLRaw.pauseAudioDevice devId 1

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
adjustVolume :: ByteString -> Double -> ByteString
adjustVolume bs volFactor
  | volFactor >= 1.0 = bs
  | volFactor <= 0.0 = BS.replicate (BS.length bs) 0
  | otherwise = unsafePerformIO $
      unsafeUseAsCString bs $ \src -> do
        let len = BS.length bs
            halfLen = len `div` 2
            volD = CDouble volFactor
            result = unsafeCreate len $ \dst -> do
              let go i
                    | i >= halfLen = pure ()
                    | otherwise = do
                        w <- peekByteOff src (i * 2) :: IO Word16
                        let unsigned = fromIntegral w :: Int
                            signed = if unsigned >= 0x8000
                                       then unsigned - 0x10000
                                       else unsigned
                            adjusted = fromIntegral
                              (round (fromIntegral signed * volD) :: Int) :: Word16
                        pokeByteOff dst (i * 2) adjusted
                        go (i + 1)
              go 0
        pure $! result

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
