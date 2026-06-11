{-|
Module      : Tourne.Error
Description : Structured application error type.

A closed sum of the failure modes Tourne produces. Replaces
@Either Text@ at every I/O boundary so callers (the TUI, log
analysers, tests) can distinguish categories instead of pattern-
matching on free-form English strings.

Design notes:

  * Closed enum with @Text@ payload — easy to construct, no orphan
    instances, no sum-of-products to maintain.
  * 'toText' gives a single-line, human-readable rendering.
  * 'errorPrefix' is the short category label (for status lines).
  * 'ToJSON' / 'FromJSON' round-trips so a future log shipper can
    serialise the @kind@ separately from the @message@.
  * No 'Exception' instance: this is a return type, never thrown.
-}
module Tourne.Error
  ( AppError (..)
  , renderError
  , errorKind
  ) where

import Relude
import Data.Aeson (FromJSON, ToJSON, (.:))
import Data.Aeson qualified as Aeson

-- | A failure produced by some Tourne subsystem. The @Text@ payload
-- is the underlying message (e.g. an exception's @show@ output, a
-- libmpg123 return code, an SDL error string). It is stored verbatim;
-- 'renderError' prepends the kind for display.
data AppError
  = HttpError         !Text  -- ^ Network/HTTP failure (DNS, connect, TLS, response).
  | JsonParseError    !Text  -- ^ Failure to decode a JSON body.
  | DecoderError      !Text  -- ^ libmpg123 failure (init, format, feed, read).
  | StreamError       !Text  -- ^ Audio stream I/O failure (open, read, cancel).
  | AudioDeviceError  !Text  -- ^ SDL2 audio device failure (open, queue, pause).
  deriving (Eq, Show, Generic)

instance ToJSON AppError where
  toJSON err = Aeson.object
    [ "kind"    Aeson..= errorKind err
    , "message" Aeson..= errorMessage err
    ]

instance FromJSON AppError where
  parseJSON = Aeson.withObject "AppError" \o -> do
    kind    <- o .: "kind"
    message <- o .: "message"
    case kind :: Text of
      "http"     -> pure (HttpError message)
      "json"     -> pure (JsonParseError message)
      "decoder"  -> pure (DecoderError message)
      "stream"   -> pure (StreamError message)
      "audio"    -> pure (AudioDeviceError message)
      other      -> fail ("unknown AppError kind: " <> toString other)

-- | The raw message payload, without the kind prefix.
errorMessage :: AppError -> Text
errorMessage = \case
  HttpError m        -> m
  JsonParseError m   -> m
  DecoderError m     -> m
  StreamError m      -> m
  AudioDeviceError m -> m

-- | Single-line, human-readable rendering. Format:
-- @<kind>: <message>@.
renderError :: AppError -> Text
renderError err = errorKind err <> ": " <> errorMessage err

-- | Short category label, suitable for a status-bar prefix or log
-- filter. Stable across messages (so a log can group by kind).
errorKind :: AppError -> Text
errorKind = \case
  HttpError _        -> "http"
  JsonParseError _   -> "json"
  DecoderError _     -> "decoder"
  StreamError _      -> "stream"
  AudioDeviceError _ -> "audio"
