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

withMpg123 :: IO a -> IO a
withMpg123 action = do
  rc <- c_mpg123_init
  if rc == 0
    then action `finally` c_mpg123_exit
    else error "Failed to initialize libmpg123"

mpg123Open :: IO (Either Text (Ptr Mpg123Handle))
mpg123Open = do
  alloca $ \perr -> do
    poke perr 0
    h <- c_mpg123_new nullPtr perr
    if h /= nullPtr
      then do
        rc <- c_mpg123_open_feed h
        if rc == 0
          then pure $ Right h
          else c_mpg123_delete h >> pure (Left "Failed to open feed")
      else do
        errCode <- peek perr
        pure $ Left $ "Failed to create handle: " <> show errCode

mpg123Close :: Ptr Mpg123Handle -> IO ()
mpg123Close h = do
  _ <- c_mpg123_close h
  c_mpg123_delete h
  pure ()

mpg123Feed :: Ptr Mpg123Handle -> ByteString -> IO (Either Text [AudioFrame])
mpg123Feed h input = do
  let feedSize = fromIntegral (BS.length input) :: CSize
  feedResult <- unsafeUseAsCStringLen input $ \(cstr, _len) -> do
    c_mpg123_feed h (castPtr cstr) feedSize

  if feedResult /= 0 && feedResult /= (-11) && feedResult /= (-10)
    then pure $ Left $ "Feed error: " <> show feedResult
    else do
      frames <- collectFrames h []
      pure $ Right frames

collectFrames :: Ptr Mpg123Handle -> [AudioFrame] -> IO [AudioFrame]
collectFrames h acc = do
  let bufSize = 16384 :: CSize
  allocaBytes (fromIntegral bufSize) $ \buf -> do
    bytesRead <- alloca $ \pBytes -> do
      poke pBytes 0
      rc <- c_mpg123_read h buf bufSize pBytes
      -- MPG123_ERR (-1) or MPG123_NEED_MORE (-10): no data available
      if rc == (-1) || rc == (-10)
        then pure 0
        else peek pBytes

    let nBytes = fromIntegral bytesRead :: Int
    if nBytes <= 0
      then pure (reverse acc)
      else do
        pcmBytes <- BS.packCStringLen (castPtr buf, nBytes)

        -- Read actual format from the decoder (handles MPG123_NEW_FORMAT changes)
        (rate, channels, _encoding) <- alloca $ \pRate -> alloca $ \pChan -> alloca $ \pEnc -> do
          _ <- c_mpg123_getformat h pRate pChan pEnc
          (,,) <$> fmap fromIntegral (peek pRate)
               <*> fmap fromIntegral (peek pChan)
               <*> peek pEnc

        let frame = AudioFrame
              { afPcmData  = pcmBytes
              , afRate     = rate
              , afChannels = channels
              , afEncoding = Mpg123EncSigned16
              }
        collectFrames h (frame : acc)

mpg123GetFormat :: Ptr Mpg123Handle -> IO (Int, Int, CInt)
mpg123GetFormat h =
  alloca $ \pRate ->
    alloca $ \pChan ->
      alloca $ \pEnc -> do
        _ <- c_mpg123_getformat h pRate pChan pEnc
        (,,) <$> fmap fromIntegral (peek pRate)
             <*> fmap fromIntegral (peek pChan)
             <*> peek pEnc

