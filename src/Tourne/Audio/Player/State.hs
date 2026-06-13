{-|
Module      : Tourne.Audio.Player.State
Description : Data types for the audio engine and per-playback state.

'AudioEngine' is the long-lived shared record: it holds every
'TVar' / 'TChan' / IORef that the audio threads coordinate on, plus
the SDL device id. 'PlaybackEnv' is the short-lived record created
fresh for each 'CmdPlay', holding the stream and decoder handles.

Also exports the @queueSafetyBytes@ constant that the decode loop
uses to decide between blocking and non-blocking stream reads.
-}
module Tourne.Audio.Player.State
  ( AudioEngine (..)
  , PlaybackEnv (..)
  , queueSafetyBytes
  ) where

import Relude
import Control.Concurrent.STM qualified as STM
import Data.IORef qualified as IORef
import Foreign.Ptr (Ptr)
import Streamly.Data.Stream.Prelude (Stream)

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
  } deriving (Generic)

-- | Per-playback environment (created fresh for each CmdPlay).
data PlaybackEnv = PlaybackEnv
  { peStream        :: !(Stream IO ByteString)
  , peDecoderHandle :: !(Ptr Decoder.Mpg123Handle)
  , peBufferTarget  :: !Int
  }

-- | Minimum SDL queue size before the decode loop falls back to a
-- blocking stream read. Below this, the loop blocks on the network
-- to keep the device fed; above it, it spins (non-blocking) to
-- avoid freezing on stalls.
queueSafetyBytes :: Word32
queueSafetyBytes = 262144  -- 256KB
