module Tourne.Audio.Stream
  ( -- * Types
    Trace

    -- * Streaming API
  , openStream'
  ) where

import Relude hiding (hFlush)
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Control.Concurrent.STM qualified as STM
import Control.Exception.Safe (tryAny)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client qualified as HC
import Streamly.Data.Stream.Prelude (Stream)
import qualified Streamly.Data.Stream.Prelude as S
import Tourne.Audio.IcyMeta
  ( IcyState, initialIcyState, parseIcyMetaint, parseStreamTitle
  , stripIcyMeta
  )
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
--
-- The @metaSink@ TVar receives parsed 'StreamTitle' values as
-- 'Just title' when a metadata block is consumed; 'Nothing' is
-- never written by this function. The TVar is reset by the caller
-- (see 'Tourne.Audio.Player.startPlayback').
openStream'
  :: Manager
  -> Text
  -> Trace
  -> STM.TVar (Maybe Text)
     -- ^ Sink for parsed ICY 'StreamTitle' values. Updated on the
     --   stream's pull thread via 'STM.atomically'. Pass a
     --   'STM.newTVarIO Nothing' if you don't want metadata.
  -> IO (Either AppError (Stream IO ByteString))
openStream' mgr urlText trace metaSink = do
  trace "[open] start" [show urlText]
  result <- tryAny do
    initReq <- HC.parseRequest (toString urlText)
    let req = initReq
          { HC.method = "GET"
          , HC.requestHeaders =
              [ ("User-Agent", "TourneRadio/0.1.0")
              , ("Accept", "*/*")
              -- Opt in to ICY metadata. Servers that support it will
              -- respond with an @icy-metaint@ header; servers that
              -- don't will simply omit the header and the stream
              -- will fall back to raw bytes (no stripping).
              , ("Icy-MetaData", "1")
              ]
          , HC.responseTimeout = HC.responseTimeoutMicro 30000000
          , HC.checkResponse = \_ _ -> pure ()  -- accept any HTTP status (radio streams redirect)
          }
    trace "[open] post-parse" []
    pure $ S.bracketIO
      (HC.responseOpen req mgr)
      HC.responseClose
      (\response ->
        let bodyReader = HC.responseBody response
            -- Read icy-metaint from the response headers (if the
            -- server is honoring our Icy-MetaData: 1 request).
            metaint   = parseIcyMetaint (HC.responseHeaders response)
            intervalN = fromMaybe 0 metaint
        in bodyReaderToStream intervalN bodyReader metaSink)
  case result of
    Right stream -> pure (Right stream)
    Left e -> pure (Left (StreamError (show e)))
  where
    -- | Build a pull-based 'Stream' of clean audio bytes. ICY
    -- metadata blocks (per the 'metaint' interval) are stripped in
    -- flight; each time a complete block is consumed the body is
    -- parsed for 'StreamTitle' and published to @metaSink@.
    --
    -- The state ('IcyState') is threaded through consecutive chunks
    -- so chunk boundaries don't confuse the stripper.
    bodyReaderToStream :: Int -> HC.BodyReader -> STM.TVar (Maybe Text) -> Stream IO ByteString
    bodyReaderToStream metaint bodyReader sink =
      S.unfoldrM step (initialIcyState metaint, True)
      where
        -- The second component of the state tuple is a one-shot flag
        -- for the first pull: we want to log the icy-metaint value
        -- exactly once, on the first pull (since the value is
        -- captured at construction, not at every step).
        step :: (IcyState, Bool)
             -> IO (Maybe (ByteString, (IcyState, Bool)))
        step (st, firstPull) = do
          when firstPull $
            trace "[open] icy-metaint"
              [show metaint]
          chunk <- HC.brRead bodyReader
          if BS.null chunk
            then pure Nothing
            else do
              let (clean, maybeMeta, st') = stripIcyMeta st chunk
              case maybeMeta of
                Just body -> do
                  let title = parseStreamTitle body
                  STM.atomically $ STM.writeTVar sink title
                  trace "[icy] meta" [maybe "empty" show title]
                Nothing -> pure ()
              -- The whole chunk may have been metadata; if so,
              -- recurse to pull the next chunk. Otherwise emit the
              -- cleaned audio.
              if BS.null clean
                then step (st', False)
                else pure (Just (clean, (st', False)))
