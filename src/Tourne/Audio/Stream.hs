module Tourne.Audio.Stream
  ( -- * Types
    Trace

    -- * Streaming API
  , openStream'
  ) where

import Relude hiding (hFlush)
import Data.ByteString qualified as BS
import Control.Exception.Safe (tryAny)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client qualified as HC
import Streamly.Data.Stream.Prelude (Stream)
import qualified Streamly.Data.Stream.Prelude as S
import Tourne.Error (AppError (..))

-- | Debug-trace callback. The Player call site passes a callback
-- derived from the 'Tracer' effect via 'withRunInIO'; tests can
-- pass @\\_ _ -> pure ()@.
type Trace = Text -> [Text] -> IO ()

--------------------------------------------------------------------------------
-- Streaming API
--------------------------------------------------------------------------------

-- | Open a radio stream and return it as a pull-based 'Stream' 'IO'
-- 'ByteString'.  The stream manages the HTTP response lifecycle via
-- 'S.bracketIO': the connection is closed when the stream ends or is
-- abandoned.
openStream' :: Manager -> Text -> Trace -> IO (Either AppError (Stream IO ByteString))
openStream' mgr urlText trace = do
  trace "[open] start" [show urlText]
  result <- tryAny do
    initReq <- HC.parseRequest (toString urlText)
    let req = initReq
          { HC.method = "GET"
          , HC.requestHeaders =
              [ ("User-Agent", "TourneRadio/0.1.0")
              , ("Accept", "*/*")
              ]
          , HC.responseTimeout = HC.responseTimeoutMicro 30000000
          }
    trace "[open] post-parse" []
    pure $ S.bracketIO
      (HC.responseOpen req mgr)
      HC.responseClose
      (\response -> let bodyReader = HC.responseBody response in bodyReaderToStream bodyReader)
  case result of
    Right stream -> pure (Right stream)
    Left e -> pure (Left (StreamError (show e)))
  where
    bodyReaderToStream :: HC.BodyReader -> Stream IO ByteString
    bodyReaderToStream bodyReader =
      S.unfoldrM step ()
      where
        step () = do
          chunk <- HC.brRead bodyReader
          if BS.null chunk
            then pure Nothing
            else pure (Just (chunk, ()))
