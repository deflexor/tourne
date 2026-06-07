module Tourne.Audio.Types where

import Relude
import Foreign.C.Types (CInt)

--------------------------------------------------------------------------------
-- libmpg123 constants
--------------------------------------------------------------------------------

mpg123Ok :: CInt
mpg123Ok = 0

mpg123Done :: CInt
mpg123Done = -1

mpg123NewFormat :: CInt
mpg123NewFormat = -11

mpg123NeedMore :: CInt
mpg123NeedMore = -10

--------------------------------------------------------------------------------
-- MPEG encoding modes
--------------------------------------------------------------------------------

data Mpg123Encoding
  = Mpg123EncSigned16    -- ^ 16-bit signed PCM
  | Mpg123EncUnsigned8   -- ^ 8-bit unsigned PCM
  | Mpg123EncSigned8     -- ^ 8-bit signed PCM
  | Mpg123EncFloat32     -- ^ 32-bit float PCM
  deriving (Eq, Show)

mpg123EncSigned16 :: CInt
mpg123EncSigned16 = 0x8000

mpg123EncFloat32 :: CInt
mpg123EncFloat32 = 0xe000

--------------------------------------------------------------------------------
-- Decoded audio frame
--------------------------------------------------------------------------------

data AudioFrame = AudioFrame
  { afPcmData    :: !ByteString      -- ^ Raw PCM bytes
  , afRate       :: !Int             -- ^ Sample rate (Hz)
  , afChannels   :: !Int             -- ^ Number of channels
  , afEncoding   :: !Mpg123Encoding
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Stream state shared between threads
--------------------------------------------------------------------------------

data StreamState = StreamState
  { ssUrl         :: !Text
  , ssConnected   :: !Bool
  , ssBuffered    :: !Int            -- ^ Bytes buffered
  , ssTotalFetched :: !Int           -- ^ Total bytes fetched
  , ssError       :: !(Maybe Text)
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Audio control commands
--------------------------------------------------------------------------------

data AudioCommand
  = CmdPlay    !Text  -- ^ Play URL
  | CmdStop
  | CmdPause
  | CmdResume
  | CmdVolume  !Double
  | CmdQuit
  deriving (Eq, Show)
