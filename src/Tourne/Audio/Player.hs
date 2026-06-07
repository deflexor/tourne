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

import Relude
import Control.Concurrent.Async (async)
import Control.Concurrent.STM qualified as STM
import Data.ByteString qualified as BS
import Data.IORef qualified as IORef
import Data.Bits ((.&.), shiftR)
import Foreign.Ptr (castPtr, nullFunPtr, nullPtr)
import Foreign.Storable (poke, peek)
import Foreign.Marshal.Alloc (alloca)
import System.Directory (doesPathExist)
import System.Environment qualified as Env
import SDL qualified
import SDL.Raw.Audio qualified as SDLRaw
import SDL.Raw.Types (AudioSpec(..))


import Tourne.Types
import Tourne.Audio.Types
import Tourne.Audio.Decoder qualified as Decoder
import Tourne.Audio.Stream qualified as Stream

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
          -- Target pre-buffer size (256KB of PCM data before starting playback)
          let bufferTargetBytes = 262144
          STM.atomically $ STM.writeTVar stateVar (Buffering 0 bufferTargetBytes)
          STM.atomically $ STM.writeTVar healthVar StreamGood

          -- Start/resume playback (0 = unpause)
          SDLRaw.pauseAudioDevice devId 0

          -- Decode loop: read stream → decode → queue to SDL2
          -- Tracks accumulated decoded bytes for buffering progress
          let decodeLoop accBytes = do
                cancelled <- STM.atomically $ STM.readTVar cancelVar
                when (not cancelled) $ do
                  -- Check for commands (CmdVolume handled inline)
                  cmd <- STM.atomically $ STM.tryReadTChan cmdChan
                  case cmd of
                    Just CmdStop  -> pure ()
                    Just CmdQuit  -> pure ()
                    _ -> do
                      -- Handle volume commands immediately even during playback
                      case cmd of
                        Just (CmdVolume vol) ->
                          STM.atomically $ STM.writeTVar volumeVar vol
                        _ -> pure ()
                      mbChunk <- Stream.readStreamChunk streamHandle
                      case mbChunk of
                        Nothing -> do
                          STM.atomically $ STM.writeTChan decodedChan Nothing
                          pure ()
                        Just chunk -> do
                          frames <- Decoder.mpg123Feed decoderHandle chunk
                          case frames of
                            Left err -> do
                              STM.atomically $ STM.writeTVar stateVar (ErrorOccurred err)
                              STM.atomically $ STM.writeTChan decodedChan Nothing
                            Right [] -> decodeLoop accBytes
                            Right frameList -> do
                              let frameBytes = sum (map (BS.length . afPcmData) frameList)
                                  newAccum   = accBytes + frameBytes
                              curVol <- STM.atomically $ STM.readTVar volumeVar
                              forM_ frameList $ \frame -> do
                                let adjusted = adjustVolume (afPcmData frame) curVol
                                queueToDevice devId adjusted

                              -- Still pre-buffering? Show progress
                              if newAccum < bufferTargetBytes
                                then do
                                  STM.atomically $ STM.writeTVar stateVar
                                    (Buffering newAccum bufferTargetBytes)
                                  decodeLoop newAccum
                                else do
                                  -- Buffer threshold reached, start playing
                                  STM.atomically $ STM.writeTVar stateVar (Playing curVol)
                                  STM.atomically $ STM.writeTVar healthVar StreamGood
                                  decodeLoop newAccum

          decodeLoop 0

          -- Cleanup
          Decoder.mpg123Close decoderHandle
          Stream.closeStream streamHandle
          SDLRaw.pauseAudioDevice devId 1

-- | Queue PCM data to SDL2 audio device using raw queueAudio
queueToDevice :: Word32 -> ByteString -> IO ()
queueToDevice devId bs = do
  let bufSize = fromIntegral (BS.length bs) :: Word32
  void $ BS.useAsCString bs $ \cstr ->
    SDLRaw.queueAudio devId (castPtr cstr) bufSize

-- | Apply volume to PCM 16-bit signed little-endian samples.
-- Properly handles negative (two's complement) sample values.
adjustVolume :: ByteString -> Double -> ByteString
adjustVolume bs volFactor
  | volFactor >= 1.0 = bs
  | volFactor <= 0.0 = BS.replicate (BS.length bs) 0
  | otherwise = BS.pack $ go 0
  where
    len = BS.length bs
    go i
      | i + 1 >= len = []
      | otherwise =
        let b0 = fromIntegral (BS.index bs i) :: Int
            b1 = fromIntegral (BS.index bs (i + 1)) :: Int
            -- Reconstruct signed 16-bit value (little-endian, two's complement)
            unsigned = b0 + b1 * 256
            signed   = if unsigned >= 0x8000 then unsigned - 0x10000 else unsigned
            adjusted = round (fromIntegral signed * volFactor) :: Int16
            lo = fromIntegral (adjusted .&. 0xFF) :: Word8
            hi = fromIntegral ((adjusted `shiftR` 8) .&. 0xFF) :: Word8
        in lo : hi : go (i + 2)

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
