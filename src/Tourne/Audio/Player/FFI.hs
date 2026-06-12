{-|
Module      : Tourne.Audio.Player.FFI
Description : Raw SDL2 audio FFI bindings used by the player.

Holds the four @foreign import@ declarations that aren't in the
@SDL2@ Haskell bindings (or whose return values we want), plus
'queueToDevice' which threads them together.

Kept separate from the rest of the player so the high-level
orchestration doesn't have to scroll past FFI noise.
-}
module Tourne.Audio.Player.FFI
  ( -- * SDL2 raw bindings
    c_sdl_get_queued_audio_size
  , c_sdl_get_audio_device_status
  , c_sdl_pause_audio_device
  , c_sdl_get_error
    -- * Higher-level helpers
  , queueToDevice
  ) where

import Relude
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Foreign.C.Types (CInt (..))
import Foreign.C.String (CString)
import Foreign.Ptr (castPtr)
import SDL.Raw.Audio qualified

-- | FFI import for SDL_GetQueuedAudioSize (not exposed by the Haskell
-- @sdl2@ bindings).
foreign import ccall "SDL_GetQueuedAudioSize"
  c_sdl_get_queued_audio_size :: Word32 -> IO Word32

-- | FFI import for SDL_GetAudioDeviceStatus.
-- Returns: 0=STOPPED, 1=PLAYING, 2=PAUSED.
foreign import ccall "SDL_GetAudioDeviceStatus"
  c_sdl_get_audio_device_status :: Word32 -> IO CInt

-- | FFI import for SDL_PauseAudioDevice (with return value).
-- Returns 0 on success, -1 on error.
foreign import ccall "SDL_PauseAudioDevice"
  c_sdl_pause_audio_device :: Word32 -> CInt -> IO CInt

-- | FFI import for SDL_GetError.
-- Returns a string describing the last SDL error.
foreign import ccall "SDL_GetError"
  c_sdl_get_error :: IO CString

-- | Queue PCM data to the SDL2 audio device using the raw
-- @SDL_QueueAudio@ binding. Returns 0 on success, -1 on error
-- (use 'c_sdl_get_error' for details).
queueToDevice :: Word32 -> ByteString -> IO CInt
queueToDevice devId bs = do
  let bufSize = fromIntegral (BS.length bs) :: Word32
  BS.useAsCString bs $ \cstr ->
    SDL.Raw.Audio.queueAudio devId (castPtr cstr) bufSize
