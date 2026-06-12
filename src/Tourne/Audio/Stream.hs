module Tourne.Audio.Stream
  ( StreamHandle
  , openStream
  , closeStream
  , readStreamChunk
  , drainStreamChunks
  ) where

import Relude hiding (hFlush)
import Control.Concurrent (forkIO)
import Control.Exception.Safe (tryAny)
import Network.HTTP.Client qualified as HC
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Control.Concurrent.STM qualified as STM
import Data.IORef qualified as IORef
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.CaseInsensitive qualified as CI
import Tourne.Error (AppError (..))
import Tourne.Http (getSharedManager)

-- | Debug-trace callback. The Player call site passes a callback
-- derived from the 'Tracer' effect via 'withRunInIO'; tests can
-- pass @\\_ _ -> pure ()@.
type Trace = Text -> [Text] -> IO ()

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

openStream :: Text -> Trace -> IO (Either AppError StreamHandle)
openStream urlText trace = do
  result <- tryAny $ do
    chan     <- STM.newTChanIO
    errRef   <- IORef.newIORef Nothing
    cancelRef <- IORef.newIORef False

    -- Create manager
    mgr <- getSharedManager

    -- Parse and configure request
    initReq <- HC.parseRequest (toString urlText)
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
      streamResult <- tryAny $ do
        let streamingReq = req { HC.checkResponse = \_ _ -> pure () }
        HC.withResponse streamingReq mgr \response -> do
          let bodyReader = HC.responseBody response
              -- Detect ICY metadata interval from response headers
              icyMetaint = case HC.responseHeaders response of
                headers -> case foldr go Nothing headers of
                  Just val -> case BSC.readInt val of
                    Just (n, _) | n > 0 -> n
                    _ -> 0
                  Nothing -> 0
                where
                  go (k, v) acc
                    | CI.foldCase k == "icy-metaint" = Just v
                    | otherwise = acc
          when (icyMetaint > 0) $
            trace "[feed] icy-metaint" [show icyMetaint]
          feedStream trace bodyReader chan cancelRef icyMetaint
      case streamResult of
        Left e -> do
          IORef.writeIORef errRef (Just $ show e)
          STM.atomically $ STM.writeTChan chan BS.empty
        Right _ -> pure ()

    pure $ StreamHandle
      { shCancel = do
          -- Only signal cancellation. The streaming thread checks
          -- this flag on each read; closing the shared HTTP manager
          -- (which it would also affect, since the manager is a
          -- process-wide singleton from Tourne.Http) would break any
          -- concurrent stream.
          IORef.writeIORef cancelRef True
      , shChunks = chan
      , shError  = errRef
      , shUrl    = urlText
      }

  case result of
    Right handle -> pure (Right handle)
    Left e -> pure (Left (StreamError (show e)))

-- | Strip ICY metadata blocks from a byte stream chunk.
-- Takes the metadata interval, remaining bytes before next metadata boundary,
-- and an input chunk. Returns (clean bytes, new remaining count).
--
-- Remaining is interpreted as:
--   > 0: bytes of clean MP3 data we can consume before next metadata block
--   = 0: at a metadata boundary (next byte is metadata length header)
--   < 0: still consuming metadata body that spanned a chunk boundary
--        (value is negative deficit of bytes still to skip)
--
-- When interval=0, passes through unchanged.
stripIcyMeta :: Int -> Int -> ByteString -> (ByteString, Int)
stripIcyMeta interval remaining bs
  | interval <= 0 = (bs, remaining)
  | BS.null bs    = (BS.empty, remaining)
  | remaining < 0 =
      -- Still consuming metadata from a previous incomplete boundary.
      -- Skip deficit bytes before counting any clean data.
      let deficit = -remaining
          skip    = min deficit (BS.length bs)
          bs'     = BS.drop skip bs
          deficit' = deficit - skip
      in if deficit' > 0
         then stripIcyMeta interval (-deficit') bs'
         else stripIcyMeta interval interval bs'
  | otherwise     = go [] remaining bs
  where
    go acc n bytes
      | BS.null bytes = (BS.concat (reverse acc), n)
      | otherwise =
          let takeLen = min n (BS.length bytes)
              (clean, rest) = BS.splitAt takeLen bytes
              acc' = clean : acc
              n' = n - takeLen
          in if n' > 0
             then go acc' n' rest
              else
                -- Hit metadata boundary: consume 1 length byte + L*16 data bytes
                case BS.uncons rest of
                  Nothing -> (BS.concat (reverse acc'), 0)
                  Just (metaLenByte, afterMeta) ->
                    let metaLen = fromIntegral metaLenByte * 16
                        availableMeta = BS.length afterMeta
                    in if availableMeta >= metaLen
                       then
                         -- Complete metadata block consumed
                         let afterMetaBlock = BS.drop metaLen afterMeta
                         in go acc' interval afterMetaBlock
                       else
                         -- Metadata body truncated by chunk boundary;
                         -- consume what's available, return deficit as negative
                         let deficit = metaLen - availableMeta
                         in (BS.concat (reverse acc'), -deficit)

-- | Feed stream data from body reader to channel.
-- Strips ICY metadata if metaInterval > 0.
feedStream :: Trace -> HC.BodyReader -> STM.TChan ByteString -> IORef.IORef Bool -> Int -> IO ()
feedStream trace bodyReader chan cancelRef metaInterval = do
  startTime <- getCurrentTime
  go BS.empty metaInterval startTime  -- start with full interval before first meta boundary
  where
    minBatchSize = 32768
    go acc remaining t0 = do
      cancelled <- IORef.readIORef cancelRef
      if cancelled
        then STM.atomically $ STM.writeTChan chan BS.empty
        else do
          chunk <- HC.brRead bodyReader
          if BS.null chunk
            then do
              unless (BS.null acc) $ do
                STM.atomically $ STM.writeTChan chan acc
                let accKb = BS.length acc `div` 1024
                trace "feed send final chunk" [show accKb <> "KB"]
              STM.atomically $ STM.writeTChan chan BS.empty  -- Signal end
            else do
              let rawSize = BS.length chunk
                  (cleanChunk, remaining') = stripIcyMeta metaInterval remaining chunk
                  strippedSize = BS.length cleanChunk
                  newAcc = acc <> cleanChunk
              when (metaInterval > 0 && rawSize /= strippedSize) $
                trace "[strip] raw"
                  [ "raw=" <> show rawSize
                  , "stripped=" <> show strippedSize
                  , "diff=" <> show (rawSize - strippedSize)
                  , "rem=" <> show remaining'
                  ]
              if BS.length newAcc >= minBatchSize
                then do
                  STM.atomically $ STM.writeTChan chan newAcc
                  let accKb = BS.length newAcc `div` 1024
                  now <- getCurrentTime
                  let elapsedMs = round (realToFrac (diffUTCTime now t0) * 1000 :: Double) :: Int
                  trace "feed send chunk"
                    [ show accKb <> "KB"
                    , "elapsed_ms=" <> show elapsedMs
                    ]
                  go BS.empty remaining' now
                else go newAcc remaining' t0

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

readStreamChunk :: StreamHandle -> IO (Maybe ByteString)
readStreamChunk StreamHandle{shChunks} = do
  chunk <- STM.atomically $ STM.readTChan shChunks
  if BS.null chunk
    then pure Nothing
    else pure (Just chunk)

-- | Non-blocking read: returns all chunks currently available in the channel
-- without waiting for more data. An empty result means no data right now.
drainStreamChunks :: StreamHandle -> IO [ByteString]
drainStreamChunks StreamHandle{shChunks} = go
  where
    go = do
      mbChunk <- STM.atomically $ STM.tryReadTChan shChunks
      case mbChunk of
        Nothing -> pure []
        Just chunk
          | BS.null chunk -> pure []
          | otherwise     -> (chunk:) <$> go

closeStream :: StreamHandle -> IO ()
closeStream StreamHandle{shCancel, shError} = do
  shCancel
  IORef.writeIORef shError (Just "Stream closed")
