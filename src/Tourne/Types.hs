module Tourne.Types where

import Relude
import Data.Aeson (FromJSON, ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Char (toLower)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment qualified as Env
import Brick.BChan (BChan)
import Tourne.Audio.Types (AudioCommand)
import Tourne.Config (Config)

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

-- | Custom JSON instances for StationId. The newtype unwraps to a
-- bare JSON string in both directions, matching how the radio-browser
-- API emits the field and how we want the cached state file to look.
-- (The default Generic-derived instances would produce
-- @{"unStationId":"abc-123"}@ instead, breaking API parsing and
-- making the cached file harder to read.)
instance ToJSON StationId where
  toJSON (StationId t) = Aeson.toJSON t

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

-- | Custom ToJSON to keep field names aligned with the radio-browser
-- API (e.g. "stationuuid", "name", "url", "tags", "bitrate", etc.)
-- and with the manual FromJSON instance below. The single field
-- "tags" stores a space-joined string to match the API shape.
instance ToJSON Station where
  toJSON Station{..} = Aeson.object
    [ "stationuuid" Aeson..= stationId
    , "name"        Aeson..= stationName
    , "url"         Aeson..= stationUrl
    , "tags"        Aeson..= Text.unwords stationTags
    , "bitrate"     Aeson..= stationBitrate
    , "codec"       Aeson..= stationCodec
    , "country"     Aeson..= stationCountry
    , "language"    Aeson..= stationLanguage
    , "clickcount"  Aeson..= stationClickCount
    ]

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

-- | Custom ToJSON to keep field names aligned with the radio-browser
-- API ("name", "stationcount") and with the manual FromJSON instance
-- below. This way state.json files round-trip cleanly and a user
-- who inspects the saved file sees familiar field names.
instance ToJSON Tag where
  toJSON Tag{..} = Aeson.object
    [ "name" Aeson..= tagName
    , "stationcount" Aeson..= tagStationCount
    ]

instance FromJSON Tag where
  parseJSON = Aeson.withObject "Tag" \o -> do
    (tagName :: Text) <- o Aeson..: "name"
    tagStationCount <- o Aeson..: "stationcount"
    pure Tag{..}

--------------------------------------------------------------------------------
-- Sorting
--------------------------------------------------------------------------------

-- | How the stations list is ordered. Selected by the user with the
-- 'o' key; defaults to 'SortByName' so the list is stable while
-- background ping results stream in.
data StationSortMode
  = SortByName
  | SortByBitrate
  | SortByPing
  deriving (Eq, Ord, Show, Generic)

instance ToJSON StationSortMode where
  toJSON = Aeson.toJSON . sortModeToText
    where
      sortModeToText :: StationSortMode -> Text
      sortModeToText SortByName    = "name"
      sortModeToText SortByBitrate = "bitrate"
      sortModeToText SortByPing    = "ping"

instance FromJSON StationSortMode where
  parseJSON = Aeson.withText "StationSortMode" \t ->
    case Text.toLower t of
      "name"    -> pure SortByName
      "bitrate" -> pure SortByBitrate
      "ping"    -> pure SortByPing
      _         -> fail ("unknown StationSortMode: " <> toString t)

-- | Cycle to the next sort mode. Used by the 'o' key.
nextSortMode :: StationSortMode -> StationSortMode
nextSortMode SortByName    = SortByBitrate
nextSortMode SortByBitrate = SortByPing
nextSortMode SortByPing    = SortByName

-- | Short human label for the active sort mode, used in the UI
-- header and the help line.
sortModeLabel :: StationSortMode -> Text
sortModeLabel SortByName    = "name"
sortModeLabel SortByBitrate = "bitrate"
sortModeLabel SortByPing    = "ping"

-- | Order stations for display according to the active sort mode.
-- Shared by the renderer (Draw) and the selection handler (Events)
-- so both always agree on order. Pure: same input -> same output.
--
-- Mode semantics:
--   * 'SortByName'    — alphabetical by station name, stable.
--   * 'SortByBitrate' — highest bitrate first; missing bitrate
--                       sorts to the end; ties broken by name.
--   * 'SortByPing'    — known ping first (ascending), unknown
--                       ping at the end sorted by name.
sortStations :: StationSortMode -> [Station] -> [Station]
sortStations mode = case mode of
  SortByName    -> sortBy (comparing stationName)
  SortByBitrate -> sortBy \a b ->
    let ka = fromMaybe 0 (stationBitrate a)
        kb = fromMaybe 0 (stationBitrate b)
    in compare kb ka <> compare (stationName a) (stationName b)
  SortByPing    -> sortBy \a b ->
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
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data StreamHealth
  = StreamGood
  | StreamDegraded !Text
  | StreamLost     !Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data AdState
  = AdInactive
  | AdMonitoring   !Double
  | AdDetected     !Double
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FailoverState
  = FailoverInactive
  | FailoverPreparing !StationId
  | FailoverSwitching !StationId
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- UI focus
--------------------------------------------------------------------------------

data Focus
  = FocusTags
  | FocusStations
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

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
  -- ^ In-prompt query text. @Just t@ while the search prompt is open;
  --   @Nothing@ otherwise. Mutates on every printable keystroke /
  --   backspace / arrow key.
  , appSearchCursor      :: !Int
  -- ^ Text-cursor position inside @appSearchText@. Only meaningful
  --   when @appSearchText = Just _@. Range: @[0, Text.length q]@.
  , appActiveFilter      :: !(Maybe Text)
  -- ^ The committed station filter. @Just t@ = stations list is
  --   filtered by @t@ (case-insensitive substring on station name);
  --   @Nothing@ = unfiltered. Set by Enter in the search prompt
  --   (or cleared by an empty query on Enter), and cleared by
  --   Esc in normal mode. Independent of @appSearchText@ — the
  --   active filter persists after the prompt is closed.
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
  , appWasPlaying        :: !Bool
  -- ^ True iff the user exited while a station was selected for playback.
  --   Used to decide whether to auto-resume on next launch. Independent of
  --   transient PlayerState (which can be Stopped right after CmdPlay fails).
  , appResumePending     :: !Bool
  -- ^ True if we should auto-fire CmdPlay on the next EvStationsLoaded
  --   (if the previously selected station is present in the list).
  --   Cleared after the resume fires, when the user changes tag, or
  --   when the user explicitly stops playback.
  , appStationsByTag     :: !(HashMap Text [Station])
  -- ^ Runtime cache of fetched stations keyed by tag name. Used to
  --   save per-tag station lists on shutdown and to seed the
  --   initial stations list for the previously selected tag on
  --   the next launch.
  , appStationSort       :: !StationSortMode
  -- ^ How the stations list is currently ordered. User-toggled with
  --   the 'o' key. Persisted across launches (see PersistedState).
  --   Defaults to 'SortByName' so the list is stable while
  --   background ping results stream in.
  , appConfig            :: !Config
  -- ^ Runtime config (API base URL, max stations, etc.). Held in
  --   AppState so background tasks spawned from event handlers can
  --   read config without needing a separate reference.
  }

data ListState = ListState
  { listSelected   :: !Int
  , listOffset     :: !Int
  , listSize       :: !Int
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

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
  | EvPersistNow
  -- ^ Triggered by the TUI when state has changed in a way worth
  --   persisting (selection, playback, volume, focus, tag change).
  --   Handler reads current AppState and writes a snapshot to disk.
  deriving (Eq, Show, Generic)
