-- | Convert a 'Stream' 'IO' 'ByteString' into the legacy 'StreamHandle'
-- shape for backward compatibility during the streaming refactor.
--
-- This module is DEPRECATED and will be removed once all consumers
-- have migrated to the new stream-based API (see Commit 6 of the
-- Streamly refactor plan).
module Tourne.Audio.Stream.Shim
  ( streamToHandle
  , streamToHandleWithUrl
  , openStreamLegacy
  ) where

import Relude

import Control.Concurrent (forkIO, killThread)
import Control.Exception.Safe (tryAny)
import Data.ByteString qualified as BS
import Control.Concurrent.STM qualified as STM
import Data.IORef qualified as IORef
import Network.HTTP.Client (Manager)
import Streamly.Data.Stream.Prelude (Stream)
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold
import Tourne.Audio.Stream (StreamHandle(..), Trace, openStream')
import Tourne.Error (AppError (..))

-- | Convert a 'Stream' 'IO' 'ByteString' into a 'StreamHandle'.
-- The stream is consumed in a forked thread; chunks are written to
-- the handle's TChan as they arrive. When the stream ends (or an
-- exception occurs), an empty ByteString is written to signal EOF.
--
-- The returned handle's 'shCancel' kills the forked thread and sets
-- the error ref.
streamToHandle :: Stream IO ByteString -> IO StreamHandle
streamToHandle stream = streamToHandleWithUrl stream "[stream]"

-- | Like 'streamToHandle' but attaches a URL label to the handle.
streamToHandleWithUrl :: Stream IO ByteString -> Text -> IO StreamHandle
streamToHandleWithUrl stream urlText = do
  chan    <- STM.newTChanIO
  errRef  <- IORef.newIORef Nothing
  _cancelRef <- IORef.newIORef False

  tid <- forkIO do
    result <- tryAny $
      S.mapM (\chunk -> STM.atomically $ STM.writeTChan chan chunk) stream
        & S.fold Fold.drain
    case result of
      Left (_ :: SomeException) -> do
        -- The error ref may already be set by shCancel, but just in
        -- case it's a real stream error, write both.
        STM.atomically $ STM.writeTChan chan BS.empty
      Right _ ->
        STM.atomically $ STM.writeTChan chan BS.empty

  pure StreamHandle
    { shCancel = do
        IORef.writeIORef errRef (Just "Stream closed")
        STM.atomically $ STM.writeTChan chan BS.empty
        killThread tid
    , shChunks = chan
    , shError  = errRef
    , shUrl    = urlText
    }

-------------------------------------------------------------------------------
-- Legacy compatibility wrapper
-------------------------------------------------------------------------------

-- | Open a radio stream via 'Tourne.Audio.Stream.openStream'' and
-- convert it into a legacy 'StreamHandle'.
--
-- This is a transitional helper for code that hasn't yet migrated
-- to the pull-based stream API.  It will be removed in Commit 6 of
-- the Streamly refactor plan.
{-# DEPRECATED openStreamLegacy "Use Tourne.Audio.Stream.openStream' instead" #-}
openStreamLegacy :: Manager -> Text -> Trace -> IO (Either AppError StreamHandle)
openStreamLegacy mgr urlText trace = do
  result <- openStream' mgr urlText trace
  case result of
    Left err -> pure (Left err)
    Right stream -> Right <$> streamToHandleWithUrl stream urlText
