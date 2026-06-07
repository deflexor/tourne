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

apiBase :: Text
apiBase = "https://de1.api.radio-browser.info"

jsonHeader :: [(HeaderName, BS.ByteString)]
jsonHeader = [(hUserAgent, "TourneRadio/0.1.0")]

-- | Make a GET request and parse JSON response
apiGet :: FromJSON a => Text -> IO (Either Text a)
apiGet path = do
  let url = apiBase <> path
  result <- try $ do
    initReq <- parseRequest (toString url)
    let req = initReq { HTTP.requestHeaders = jsonHeader }
    mgr <- getSharedManager
    response <- HTTP.httpLbs req mgr
    pure (Aeson.eitherDecode (HTTP.responseBody response))
  case result of
    Right (Right val) -> pure (Right val)
    Right (Left parseErr) -> pure (Left ("JSON parse error: " <> toText parseErr))
    Left (e :: SomeException) -> pure (Left (show e))

--------------------------------------------------------------------------------
-- Tag fetching
--------------------------------------------------------------------------------

-- | Fetch all tags from the API
fetchTags :: IO (Either Text [Tag])
fetchTags = do
  result <- apiGet "/json/tags" :: IO (Either Text [Tag])
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
fetchStationsByTag :: Text -> Int -> IO (Either Text [Station])
fetchStationsByTag tag limit = do
  let path = "/json/stations/bytag/" <> tag
  result <- apiGet path :: IO (Either Text [Station])
  case result of
    Right stations -> do
      let sortedStations = sortStations stations
      pure $ Right $ take limit sortedStations
    Left e -> pure $ Left e

-- | Search stations by name
searchStations :: Text -> IO (Either Text [Station])
searchStations query = do
  let path = "/json/stations/byname/" <> query
  result <- apiGet path :: IO (Either Text [Station])
  case result of
    Right stations -> pure $ Right $ sortStations stations
    Left e         -> pure $ Left e

-- | Sort stations by click count (descending); stations with no click count
-- are treated as 0 so they sort last.
sortStations :: [Station] -> [Station]
sortStations = sortOn (Down . fromMaybe 0 . stationClickCount)
