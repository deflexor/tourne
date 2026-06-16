{-|
Module      : Tourne.Audio.Player.State
Description : Data types for the audio engine and per-playback state.

'AudioEngine' is the long-lived shared record: it holds every
'TVar' / 'TChan' / IORef that the audio threads coordinate on, plus
the SDL device id. 'PlaybackEnv' is the short-lived record created
fresh for each 'CmdPlay', holding the stream and decoder handles.

The 'aeRateVar' and 'aeChannelsVar' fields are populated from the
'obtained' AudioSpec when the SDL device is opened, and are read by
the decode loop to calibrate the rate limiter against the actual
playback rate (so a sample rate mismatch doesn't cause the SDL
queue to oscillate).
-}
module Tourne.Audio.Player.State
  ( AudioEngine (..)
  , PlaybackEnv (..)
  ) where

import Relude
import Control.Concurrent.STM qualified as STM
import Data.IORef qualified as IORef
import Foreign.Ptr (Ptr)

import Tourne.Audio.Decoder qualified as Decoder
import Tourne.Audio.Types (AudioCommand)
import Tourne.Types (PlayerState, StreamHealth)

-- | Long-lived engine. All shared state for the audio threads.
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
  , aeIcyMetaVar    :: !(STM.TVar (Maybe Text))
  } deriving (Generic)

-- | Per-playback environment (created fresh for each CmdPlay).
data PlaybackEnv = PlaybackEnv
  { peDecoderHandle :: !(Ptr Decoder.Mpg123Handle)
  , peBufferTarget  :: !Int
  , peMp3Queue      :: !(STM.TBQueue ByteString)
  , peReaderDone    :: !(STM.TVar Bool)
  }
