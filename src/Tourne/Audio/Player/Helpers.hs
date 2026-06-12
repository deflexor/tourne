{-|
Module      : Tourne.Audio.Player.Helpers
Description : Pure(ish) helpers used by the audio decode loop.

'adjustVolume' is in @IO@ because the body uses 'unsafeCreate'
(an FFI primitive) and reads/pokes the input buffer. The
early-return branches are constant-time and allocation-free.

'drainChan' is a small STM helper used by 'CmdStop' / 'CmdQuit'
to clear the decoded-audio channel.
-}
module Tourne.Audio.Player.Helpers
  ( adjustVolume
  , drainChan
  ) where

import Relude
import Data.ByteString qualified as BS
import Data.ByteString.Internal (unsafeCreate)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Foreign.C.Types (CDouble (..))
import Foreign.Storable (peekByteOff, pokeByteOff)
import Control.Concurrent.STM qualified as STM

-- | Apply volume to PCM 16-bit signed little-endian samples.
-- Uses pointer-based processing ('unsafeCreate') to avoid list
-- allocation and GC pressure that caused audio stuttering.
--
-- Lives in @IO@ because the body calls 'unsafeCreate'; the
-- early-return branches are constant-time and allocation-free.
adjustVolume :: ByteString -> Double -> IO ByteString
adjustVolume bs volFactor
  | volFactor >= 1.0 = pure bs
  | volFactor <= 0.0 = pure (BS.replicate (BS.length bs) 0)
  | otherwise = do
      let len     = BS.length bs
          halfLen = len `div` 2
          volD    = CDouble volFactor
      unsafeUseAsCString bs $ \src ->
        pure $ unsafeCreate len $ \dst ->
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
          in go 0

-- | Drain all remaining items from a channel.
drainChan :: STM.TChan a -> IO ()
drainChan chan = STM.atomically $ go
  where
    go = do
      mb <- STM.tryReadTChan chan
      case mb of
        Nothing -> pure ()
        Just _  -> go
