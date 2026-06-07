module Tourne.Types where

import Relude
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Char (toLower)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment qualified as Env
import Brick.BChan (BChan)
import Tourne.Audio.Types (AudioCommand)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Enable per-iteration debug logging to stderr. Off by default; set
-- @TOURNE_DEBUG=1@ (or =true) at launch to enable.
debugEnabled :: Bool
debugEnabled = unsafePerformIO do
  v <- Env.lookupEnv "TOURNE_DEBUG"
  let norm = map toLower <$> v
  pure (norm == Just "1" || norm == Just "true")

--------------------------------------------------------------------------------
-- Identifiers
--------------------------------------------------------------------------------

newtype StationId = StationId { unStationId :: Text }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON StationId where
  parseJSON v = StationId <$> Aeson.parseJSON v

--------------------------------------------------------------------------------
-- Radio stations & tags
--------------------------------------------------------------------------------

data Station = Station
  { stationId          :: !StationId
  , stationName        :: !Text
  , stationUrl         :: !Text
  , stationTags        :: ![Text]
  , stationBitrate     :: !(Maybe Int)
  , stationCodec       :: !(Maybe Text)
  , stationCountry     :: !(Maybe Text)
  , stationLanguage    :: !(Maybe Text)
  , stationPing        :: !(Maybe Double)
  , stationClickCount  :: !(Maybe Int)
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Station where
  parseJSON = Aeson.withObject "Station" \o -> do
    stationId        <- o Aeson..: "stationuuid"
    stationName      <- o Aeson..: "name"
    stationUrl       <- o Aeson..: "url"
    (tagsStr :: Text) <- o Aeson..: "tags"
    let stationTags  = words (Text.toLower tagsStr)
    stationBitrate   <- o Aeson..: "bitrate"
    stationCodec     <- o Aeson..: "codec"
    stationCountry   <- o Aeson..: "country"
    stationLanguage  <- o Aeson..: "language"
    stationPing      <- pure Nothing
    stationClickCount <- o Aeson..: "clickcount"
    pure Station{..}

data Tag = Tag
  { tagName         :: !Text
  , tagStationCount :: !Int
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Tag where
  parseJSON = Aeson.withObject "Tag" \o -> do
    (tagName :: Text) <- o Aeson..: "name"
    tagStationCount <- o Aeson..: "stationcount"
    pure Tag{..}

--------------------------------------------------------------------------------
-- Sorting
--------------------------------------------------------------------------------

-- | Order stations for display: known ping first (ascending), then unknown
-- ping sorted by name. Shared by the renderer (Draw) and the selection
-- handler (Events) so both always agree on order.
sortStationsByPing :: [Station] -> [Station]
sortStationsByPing = sortBy \a b ->
  case (stationPing a, stationPing b) of
    (Nothing, Nothing) -> compare (stationName a) (stationName b)
    (Nothing, Just _)  -> GT
    (Just _, Nothing)  -> LT
    (Just pa, Just pb) -> compare pa pb

--------------------------------------------------------------------------------
-- Player state
--------------------------------------------------------------------------------

data PlayerState
  = Stopped
  | Connecting     !Text          -- ^ URL being connected to
  | Buffering      !Int !Int      -- ^ (buffered_bytes, total_bytes)
  | Playing        !Double        -- ^ current volume level (0-1)
  | Paused
  | ErrorOccurred  !Text
  deriving (Eq, Show, Generic)

data StreamHealth
  = StreamGood
  | StreamDegraded !Text
  | StreamLost     !Text
  deriving (Eq, Show, Generic)

data AdState
  = AdInactive
  | AdMonitoring   !Double
  | AdDetected     !Double
  deriving (Eq, Show, Generic)

data FailoverState
  = FailoverInactive
  | FailoverPreparing !StationId
  | FailoverSwitching !StationId
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- UI focus
--------------------------------------------------------------------------------

data Focus
  = FocusTags
  | FocusStations
  deriving (Eq, Ord, Show, Generic)

--------------------------------------------------------------------------------
-- Global application state
--------------------------------------------------------------------------------

data AppState = AppState
  { appTags              :: ![Tag]
  , appStations          :: ![Station]
  , appCurrentTag        :: !(Maybe Text)
  , appSelectedStation   :: !(Maybe StationId)
  , appPlayerState       :: !PlayerState
  , appFocus             :: !Focus
  , appSearchText        :: !(Maybe Text)
  , appAdState           :: !AdState
  , appFailoverState     :: !FailoverState
  , appStreamHealth      :: !StreamHealth
  , appErrorMessage      :: !(Maybe Text)
  , appPingResults       :: !(HashMap Text Double)
  , appVolume            :: !Double
  , appTagsListState     :: !ListState
  , appStationsListState :: !ListState
  , appEventChan         :: !(Maybe (BChan AppEvent))
  , appLoadingStations   :: !Bool
  , appAudioCommand      :: !(Maybe (AudioCommand -> IO ()))
  , appStationsVar       :: !(Maybe (TVar [Station]))
  }

data ListState = ListState
  { listSelected   :: !Int
  , listOffset     :: !Int
  , listSize       :: !Int
  } deriving (Eq, Show, Generic)

defaultListState :: ListState
defaultListState = ListState 0 0 0

--------------------------------------------------------------------------------
-- Event types for the Brick event loop
--------------------------------------------------------------------------------

data AppEvent
  = EvTick
  | EvTagSelected      !Text
  | EvStationSelected  !StationId
  | EvPlayerUpdate     !PlayerState
  | EvStreamHealth     !StreamHealth
  | EvPingUpdate       !StationId !(Either Text Double)
  | EvAdUpdate         !AdState
  | EvStationsLoaded   ![Station]
  | EvTagsLoaded       ![Tag]
  | EvError            !Text
  | EvVolumeUpdate     !Double
  | EvShutdown
  deriving (Eq, Show, Generic)
