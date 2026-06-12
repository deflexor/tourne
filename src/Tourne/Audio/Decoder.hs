{-# LANGUAGE CApiFFI #-}

module Tourne.Audio.Decoder
  ( Mpg123Handle
  , withMpg123
  , mpg123Open
  , mpg123Close
  , mpg123Feed
  , mpg123GetFormat
  ) where

import Relude
import Foreign.C.Types
  ( CInt(..), CLong(..), CSize(..), CUChar(..), CChar )
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Data.ByteString qualified as BS
import Data.ByteString.Internal (ByteString(..), toForeignPtr)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Control.Exception (finally)

import Tourne.Audio.Types
import Tourne.Error (AppError (..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Mpg123Handle
data Mpg123Library

type Mpg123HandlePtr = Ptr Mpg123Handle
type Mpg123LibraryPtr = Ptr Mpg123Library

--------------------------------------------------------------------------------
-- Library initialization
--------------------------------------------------------------------------------

foreign import capi "mpg123.h mpg123_init"
  c_mpg123_init :: IO CInt

foreign import capi "mpg123.h mpg123_exit"
  c_mpg123_exit :: IO ()

--------------------------------------------------------------------------------
-- Handle management
--------------------------------------------------------------------------------

foreign import capi "mpg123.h mpg123_new"
  c_mpg123_new :: Ptr CChar -> Ptr CInt -> IO (Ptr Mpg123Handle)

foreign import capi "mpg123.h mpg123_delete"
  c_mpg123_delete :: Ptr Mpg123Handle -> IO ()

foreign import capi "mpg123.h mpg123_open_feed"
  c_mpg123_open_feed :: Ptr Mpg123Handle -> IO CInt

foreign import capi "mpg123.h mpg123_close"
  c_mpg123_close :: Ptr Mpg123Handle -> IO CInt

foreign import capi "mpg123.h mpg123_format"
  c_mpg123_format :: Ptr Mpg123Handle -> CLong -> CInt -> CInt -> IO CInt

foreign import capi "mpg123.h mpg123_getformat"
  c_mpg123_getformat :: Ptr Mpg123Handle -> Ptr CLong -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import capi "mpg123.h mpg123_feed"
  c_mpg123_feed :: Ptr Mpg123Handle -> Ptr CUChar -> CSize -> IO CInt

foreign import capi "mpg123.h mpg123_read"
  c_mpg123_read :: Ptr Mpg123Handle -> Ptr CUChar -> CSize -> Ptr CSize -> IO CInt

foreign import capi "mpg123.h mpg123_decode_frame"
  c_mpg123_decode_frame :: Ptr Mpg123Handle -> Ptr CLong -> Ptr (Ptr CUChar) -> Ptr CSize -> IO CInt

--------------------------------------------------------------------------------
-- Safe API
--------------------------------------------------------------------------------

withMpg123 :: IO a -> IO (Either AppError a)
withMpg123 action = do
  rc <- c_mpg123_init
  if rc == 0
    then (Right <$> action) `finally` c_mpg123_exit
    else pure (Left (DecoderError "Failed to initialize libmpg123"))

mpg123Open :: IO (Either AppError (Ptr Mpg123Handle))
mpg123Open = do
  alloca $ \perr -> do
    poke perr 0
    h <- c_mpg123_new nullPtr perr
    if h /= nullPtr
      then do
        rc <- c_mpg123_open_feed h
        if rc /= 0
          then c_mpg123_delete h >> pure (Left (DecoderError "Failed to open feed"))
          else do
            -- Force the output format to match the SDL2 device we open
            -- in Tourne.Audio.Player (44100 Hz, 2 channels, signed 16-bit).
            -- Without this, mpg123 may keep buffering bytes forever
            -- waiting for the input to match its default format, or
            -- happily hand us a different rate/channels/encoding that
            -- the SDL2 queue misinterprets. mpg123_format returns
            -- MPG123_OK on success and -10/-11 if the format is
            -- rejected; we treat any non-zero as a hard error so the
            -- failure mode is loud rather than silent no-audio.
            let rate     = 44100 :: CLong
                channels = 2 :: CInt
                encoding = mpg123EncSigned16 :: CInt
            fmtRc <- c_mpg123_format h rate channels encoding
            if fmtRc == 0
              then pure $ Right h
              else do
                c_mpg123_delete h
                pure $ Left $ DecoderError $
                  "mpg123_format rejected 44100/2/S16 (rc=" <> show fmtRc <> ")"
      else do
        errCode <- peek perr
        pure $ Left $ DecoderError $ "Failed to create handle: " <> show errCode

mpg123Close :: Ptr Mpg123Handle -> IO ()
mpg123Close h = do
  _ <- c_mpg123_close h
  c_mpg123_delete h
  pure ()

mpg123Feed :: Ptr Mpg123Handle -> ByteString -> IO (Either AppError [AudioFrame])
mpg123Feed h input = do
  let feedSize = fromIntegral (BS.length input) :: CSize
  feedResult <- unsafeUseAsCStringLen input $ \(cstr, _len) -> do
    c_mpg123_feed h (castPtr cstr) feedSize

  if feedResult /= 0 && feedResult /= (-11) && feedResult /= (-10)
    then pure $ Left $ DecoderError $ "Feed error: " <> show feedResult
    else do
      frames <- collectFrames h []
      pure $ Right frames

collectFrames :: Ptr Mpg123Handle -> [AudioFrame] -> IO [AudioFrame]
collectFrames h acc = readOnce h acc

-- | Inner loop: one call to 'mpg123_read', dispatch on its return code.
-- Loop on MPG123_NEW_FORMAT (-11) and stop on MPG123_NEED_MORE (-10)
-- or MPG123_DONE (-1). 'mpg123_read' may return nBytes > 0 alongside
-- a NEW_FORMAT code (the frame was decoded at the previous format
-- and the new one starts after it); we keep the frame and continue.
readOnce :: Ptr Mpg123Handle -> [AudioFrame] -> IO [AudioFrame]
readOnce h acc = do
  let bufSize = 16384 :: CSize
  allocaBytes (fromIntegral bufSize) $ \(buf :: Ptr CUChar) -> do
    (rc, bytesRead) <- alloca $ \pBytes -> do
      poke pBytes 0
      rc <- c_mpg123_read h buf bufSize pBytes
      n <- peek pBytes
      pure (rc, n)

    let nBytes = fromIntegral bytesRead :: Int
    case (rc :: CInt, nBytes) of
      -- No more decoded frames available in the current input;
      -- the caller (or the next feed) will produce more.
      (-1, _)        -> pure (reverse acc)
      (-10, _)       -> pure (reverse acc)
      (-11, 0)       -> readOnce h acc  -- format changed but no data this call
      (-11, _)       -> commitFrame h buf bytesRead acc >>= readOnce h
      (0, n) | n > 0 -> commitFrame h buf bytesRead acc >>= readOnce h
      _              -> pure (reverse acc)

-- | Materialise the bytes 'mpg123_read' just wrote into 'buf' into
-- an 'AudioFrame' after consulting 'mpg123_getformat'. Prepends to 'acc'.
commitFrame
  :: Ptr Mpg123Handle
  -> Ptr CUChar
  -> CSize
  -> [AudioFrame]
  -> IO [AudioFrame]
commitFrame h buf bytesRead acc = do
  let nBytes = fromIntegral bytesRead :: Int
  pcmBytes <- BS.packCStringLen (castPtr buf, nBytes)
  (rate, channels, encCInt) <- alloca $ \pRate -> alloca $ \pChan -> alloca $ \pEnc -> do
    _ <- c_mpg123_getformat h pRate pChan pEnc
    (,,) <$> fmap fromIntegral (peek pRate)
         <*> fmap fromIntegral (peek pChan)
         <*> peek pEnc
  -- Default to 16-bit signed if the decoder reports an unknown
  -- encoding; this is the format the SDL2 device is opened in
  -- (see Audio/Player.hs initAudio). A wrong-bytes-per-frame
  -- assumption is loud (no audio) rather than silent.
  let encoding = fromMaybe Mpg123EncSigned16 (cIntToMpg123Enc encCInt)
      frame = AudioFrame
        { afPcmData  = pcmBytes
        , afRate     = rate
        , afChannels = channels
        , afEncoding = encoding
        }
  pure (frame : acc)

mpg123GetFormat :: Ptr Mpg123Handle -> IO (Int, Int, CInt)
mpg123GetFormat h =
  alloca $ \pRate ->
    alloca $ \pChan ->
      alloca $ \pEnc -> do
        _ <- c_mpg123_getformat h pRate pChan pEnc
        (,,) <$> fmap fromIntegral (peek pRate)
             <*> fmap fromIntegral (peek pChan)
             <*> peek pEnc

