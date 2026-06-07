module Tourne.Audio.Stream
  ( StreamHandle
  , openStream
  , closeStream
  , readStreamChunk
  ) where

import Relude
import Control.Concurrent (forkIO)
import Control.Exception (try)
import Network.HTTP.Conduit qualified as HTTP
import Network.HTTP.Client qualified as HC
import Data.ByteString qualified as BS
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TChan qualified as STM
import Data.IORef qualified as IORef

--------------------------------------------------------------------------------
-- Stream handle
--------------------------------------------------------------------------------

data StreamHandle = StreamHandle
  { shCancel    :: !(IO ())
  , shChunks    :: !(STM.TChan ByteString)
  , shError     :: !(IORef.IORef (Maybe Text))
  , shUrl       :: !Text
  } deriving (Generic)

--------------------------------------------------------------------------------
-- Open a radio stream
--------------------------------------------------------------------------------

openStream :: Text -> IO (Either Text StreamHandle)
openStream urlText = do
  result <- try $ do
    chan     <- STM.newTChanIO
    errRef   <- IORef.newIORef Nothing
    cancelRef <- IORef.newIORef False

    -- Create manager
    mgr <- HTTP.newManager HTTP.tlsManagerSettings

    -- Parse and configure request
    initReq <- HTTP.parseRequest (toString urlText)
    let req = initReq
          { HC.method = "GET"
           , HC.requestHeaders =
               [ ("User-Agent", "TourneRadio/0.1.0")
               , ("Accept", "*/*")
               ]
          , HC.responseTimeout = HC.responseTimeoutMicro 30000000
          }

    -- Fork thread to stream data
    _ <- forkIO $ do
      -- Open streaming connection
      streamResult <- try $ do
        let streamingReq = req { HC.checkResponse = \_ _ -> pure () }
        HC.withResponse streamingReq mgr \response -> do
          let bodyReader = HC.responseBody response
          feedStream bodyReader chan cancelRef
      case streamResult of
        Left (e :: SomeException) -> do
          IORef.writeIORef errRef (Just $ show e)
          STM.atomically $ STM.writeTChan chan BS.empty
        Right _ -> pure ()

    pure $ StreamHandle
      { shCancel = do
          IORef.writeIORef cancelRef True
          HTTP.closeManager mgr
      , shChunks = chan
      , shError  = errRef
      , shUrl    = urlText
      }

  case result of
    Right handle -> pure (Right handle)
    Left (e :: SomeException) -> pure (Left $ show e)

-- | Feed stream data from body reader to channel
feedStream :: HC.BodyReader -> STM.TChan ByteString -> IORef.IORef Bool -> IO ()
feedStream bodyReader chan cancelRef = go
  where
    go = do
      cancelled <- IORef.readIORef cancelRef
      if cancelled
        then STM.atomically $ STM.writeTChan chan BS.empty
        else do
          chunk <- HC.brRead bodyReader
          if BS.null chunk
            then STM.atomically $ STM.writeTChan chan BS.empty  -- Signal end
            else do
              STM.atomically $ STM.writeTChan chan chunk
              go

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

readStreamChunk :: StreamHandle -> IO (Maybe ByteString)
readStreamChunk StreamHandle{shChunks} = do
  chunk <- STM.atomically $ STM.readTChan shChunks
  if BS.null chunk
    then pure Nothing
    else pure (Just chunk)

closeStream :: StreamHandle -> IO ()
closeStream StreamHandle{shCancel, shError} = do
  shCancel
  IORef.writeIORef shError (Just "Stream closed")
