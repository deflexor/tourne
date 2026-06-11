module Tourne.RadioBrowser
  ( fetchTags
  , fetchStationsByTag
  , searchStations
  , StationQuery(..)
  ) where

import Relude
import Control.Exception (try)
import Network.HTTP.Conduit qualified as HTTP
import Network.HTTP.Simple
  ( httpJSON, parseRequest, setRequestQueryString
  , getResponseBody, setRequestHeader
  )
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Network.HTTP.Types.Header (hUserAgent, HeaderName)
import Network.HTTP.Types.URI (QueryItem)
import Tourne.Config (Config, configApiBaseUrl)
import Tourne.Error (AppError (..))
import Tourne.Types
import Tourne.Http (getSharedManager)

--------------------------------------------------------------------------------
-- Query parameters
--------------------------------------------------------------------------------

data StationQuery = StationQuery
  { sqTag      :: !(Maybe Text)
  , sqName     :: !(Maybe Text)
  , sqLimit    :: !(Maybe Int)
  , sqOrder    :: !(Maybe Text)
  , sqReverse  :: !Bool
  } deriving (Eq, Show)

defaultQuery :: StationQuery
defaultQuery = StationQuery
  { sqTag     = Nothing
  , sqName    = Nothing
  , sqLimit   = Just 100
  , sqOrder   = Just "clickcount"
  , sqReverse = True
  }

--------------------------------------------------------------------------------
-- HTTP helpers
--------------------------------------------------------------------------------

-- | All functions in this module take a 'Config' so the API base URL
-- is read from the single source of truth ('configApiBaseUrl') instead
-- of being hardcoded here.

jsonHeader :: [(HeaderName, BS.ByteString)]
jsonHeader = [(hUserAgent, "TourneRadio/0.1.0")]

-- | Make a GET request and parse JSON response. Distinguishes HTTP
-- failures (network/TLS) from JSON parse failures.
apiGet :: FromJSON a => Text -> Text -> IO (Either AppError a)
apiGet base path = do
  let url = base <> path
  result <- try $ do
    initReq <- parseRequest (toString url)
    let req = initReq { HTTP.requestHeaders = jsonHeader }
    mgr <- getSharedManager
    response <- HTTP.httpLbs req mgr
    pure (Aeson.eitherDecode (HTTP.responseBody response))
  case result of
    Right (Right val) -> pure (Right val)
    Right (Left parseErr) -> pure (Left (JsonParseError (toText parseErr)))
    Left (e :: SomeException) -> pure (Left (HttpError (show e)))

--------------------------------------------------------------------------------
-- Tag fetching
--------------------------------------------------------------------------------

-- | Fetch all tags from the API
fetchTags :: Config -> IO (Either AppError [Tag])
fetchTags cfg = do
  result <- apiGet (configApiBaseUrl cfg) "/json/tags" :: IO (Either AppError [Tag])
  case result of
    Right tags -> pure $ Right $ take 200 $ sortTags tags
    Left e     -> pure $ Left e

-- | Sort tags by station count (most popular first)
sortTags :: [Tag] -> [Tag]
sortTags = sortOn (Down . tagStationCount)

--------------------------------------------------------------------------------
-- Station fetching
--------------------------------------------------------------------------------

-- | Fetch stations by tag
fetchStationsByTag :: Config -> Text -> Int -> IO (Either AppError [Station])
fetchStationsByTag cfg tag limit = do
  let path = "/json/stations/bytag/" <> tag
  result <- apiGet (configApiBaseUrl cfg) path :: IO (Either AppError [Station])
  case result of
    Right stations -> do
      let sortedStations = sortByClickCount stations
      pure $ Right $ take limit sortedStations
    Left e -> pure $ Left e

-- | Search stations by name
searchStations :: Config -> Text -> IO (Either AppError [Station])
searchStations cfg query = do
  let path = "/json/stations/byname/" <> query
  result <- apiGet (configApiBaseUrl cfg) path :: IO (Either AppError [Station])
  case result of
    Right stations -> pure $ Right $ sortByClickCount stations
    Left e         -> pure $ Left e

-- | Sort stations by click count (descending); stations with no click count
-- are treated as 0 so they sort last. This is the API-result sort
-- applied at fetch time and is independent of the user-toggled
-- display sort mode (see 'Tourne.Types.sortStations').
sortByClickCount :: [Station] -> [Station]
sortByClickCount = sortOn (Down . fromMaybe 0 . stationClickCount)
