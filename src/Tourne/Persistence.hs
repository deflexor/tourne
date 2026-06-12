{-|
Module      : Tourne.Persistence
Description : On-disk state cache for tags, stations, and session restore.

Persists a small JSON document to @\$XDG_CONFIG_HOME\/tourne\/state.json@
(default @~\/.config\/tourne\/state.json@) so the TUI can:

  * Show tags and the last-viewed tag's stations instantly on launch,
    without waiting for the radio-browser API.
  * Auto-resume playback of the previously selected station if the user
    exited while it was playing.

The file is written atomically (write to @.tmp@, then @renameFile@) so a
crash mid-write never leaves a half-written state file. All disk errors
are best-effort: a failure to load returns an empty state, and a failure
to save is logged to stderr but does not crash the app.
-}
module Tourne.Persistence
  ( -- * State type
    PersistedState (..)
  , defaultPersistedState
  , emptyPersistedState
    -- * Paths
  , stateFilePath
    -- * IO
  , loadPersistedState
  , savePersistedState
    -- * Pure helpers (testable without IO)
  , encodePersistedState
  , decodePersistedState
  , appToPersisted
  ) where

import Relude
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import System.Directory
  ( XdgDirectory(..), createDirectoryIfMissing, doesFileExist
  , getXdgDirectory, renameFile
  )
import System.IO (hPutStrLn)
import System.IO.Error (tryIOError)

import Tourne.Error (AppError (..), renderError)
import Tourne.Types
  ( AppState (..), Focus (..), ListState (..), Station (..), StationId, Tag
  , StationSortMode (..), lookupStation
  )

--------------------------------------------------------------------------------
-- Persisted state
--------------------------------------------------------------------------------

-- | Snapshot of the user-visible state worth restoring on next launch.
--
-- Intentionally small: only the data needed to recreate the previous
-- session, not the full runtime AppState.
data PersistedState = PersistedState
  { psVersion          :: !Int          -- ^ Schema version, for future migrations
  , psTags             :: ![Tag]        -- ^ Cached tag list
  , psStationsByTag    :: !(HashMap Text [Station])  -- ^ Per-tag cached station list
  , psCurrentTag       :: !(Maybe Text) -- ^ Last selected tag
  , psSelectedStation  :: !(Maybe StationId) -- ^ Last selected station
  , psVolume           :: !Double       -- ^ Last volume
  , psFocus            :: !Focus        -- ^ Last focused pane
  , psTagsCursor       :: !ListState    -- ^ Tag list cursor
  , psStationsCursor   :: !ListState    -- ^ Station list cursor
  , psWasPlaying       :: !Bool         -- ^ True if user exited while playing
  , psResumeStationUrl :: !(Maybe Text) -- ^ Cached URL of the playing station
                                          --   (so resume can work even if
                                          --   the stations list is empty)
  , psStationSort      :: !StationSortMode -- ^ User-selected stations sort mode
  } deriving (Eq, Show, Generic)

instance Aeson.ToJSON PersistedState

-- | Custom 'FromJSON' for forward compatibility.
--
-- Notes:
--   * The Generic-derived 'ToJSON' encodes field names verbatim
--     (e.g. @"psVersion"@, @"psCurrentTag"@), so the keys below match
--     the on-disk format produced by previous versions.
--   * The 'psStationSort' field was added in schema v2; older v1
--     state files do not have it, so we use '.:?' to fall back to
--     'SortByName' when missing. This keeps existing v1 cache files
--     loadable after upgrade.
instance Aeson.FromJSON PersistedState where
  parseJSON = Aeson.withObject "PersistedState" \o -> do
    psVersion          <- o Aeson..: "psVersion"
    psTags             <- o Aeson..:  "psTags"
    psStationsByTag    <- o Aeson..:  "psStationsByTag"
    psCurrentTag       <- o Aeson..:  "psCurrentTag"
    psSelectedStation  <- o Aeson..:  "psSelectedStation"
    psVolume           <- o Aeson..:  "psVolume"
    psFocus            <- o Aeson..:  "psFocus"
    psTagsCursor       <- o Aeson..:  "psTagsCursor"
    psStationsCursor   <- o Aeson..:  "psStationsCursor"
    psWasPlaying       <- o Aeson..:  "psWasPlaying"
    psResumeStationUrl <- o Aeson..:  "psResumeStationUrl"
    psStationSort      <- o Aeson..:? "psStationSort" Aeson..!= SortByName
    pure PersistedState{..}

-- | Schema version constant. Bump if PersistedState's shape changes.
-- v2 adds 'psStationSort' (with backward-compatible parsing).
currentSchemaVersion :: Int
currentSchemaVersion = 2

-- | Default empty state used both as a safe fallback when the file is
-- missing/malformed and as the initial value for a fresh install.
defaultPersistedState :: PersistedState
defaultPersistedState = PersistedState
  { psVersion          = currentSchemaVersion
  , psTags             = []
  , psStationsByTag    = mempty
  , psCurrentTag       = Nothing
  , psSelectedStation  = Nothing
  , psVolume           = 0.8
  , psFocus            = FocusTags
  , psTagsCursor       = ListState 0 0 0
  , psStationsCursor   = ListState 0 0 0
  , psWasPlaying       = False
  , psResumeStationUrl = Nothing
  , psStationSort      = SortByName
  }

-- | Alias for defaultPersistedState for clarity at call sites.
emptyPersistedState :: PersistedState
emptyPersistedState = defaultPersistedState

--------------------------------------------------------------------------------
-- Paths
--------------------------------------------------------------------------------

-- | The canonical XDG config path for this app's state file.
stateFilePath :: IO FilePath
stateFilePath = do
  dir <- getXdgDirectory XdgConfig "tourne"
  pure (dir <> "/state.json")

--------------------------------------------------------------------------------
-- Pure encode/decode
--------------------------------------------------------------------------------

-- | Serialize a PersistedState to JSON bytes.
encodePersistedState :: PersistedState -> BSL.ByteString
encodePersistedState = Aeson.encode

-- | Parse a PersistedState from JSON bytes.
-- Returns 'Left' with an error message on failure (e.g. corrupt file,
-- version mismatch, missing required fields).
--
-- Schema v1 files (no 'stationSort' field) are accepted for backward
-- compatibility: the custom 'FromJSON' instance defaults
-- 'psStationSort' to 'SortByName' when the field is missing.
decodePersistedState :: BSL.ByteString -> Either AppError PersistedState
decodePersistedState bs = case Aeson.eitherDecode bs of
  Right s
    | psVersion s == currentSchemaVersion -> Right s
    | psVersion s == currentSchemaVersion - 1 -> Right s
    | otherwise -> Left
        (JsonParseError ("unsupported schema version: " <> show (psVersion s)))
  Left err -> Left (JsonParseError (toText err))

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

-- | Load persisted state from disk. If the file is missing, unreadable,
-- or malformed, returns 'emptyPersistedState' and logs a notice to
-- stderr. Never throws.
loadPersistedState :: IO PersistedState
loadPersistedState = do
  path <- stateFilePath
  result <- tryIOError do
    exists <- doesFileExist path
    if not exists
      then pure (Right emptyPersistedState)
      else do
        bs <- BSL.readFile path
        pure (decodePersistedState bs)
  case result of
    Right (Right s) -> pure s
    Right (Left err) -> do
      hPutStrLn stderr
        ("[tourne] state file at " <> toString path
         <> " could not be parsed (" <> toString (renderError err)
         <> "); using empty state")
      pure emptyPersistedState
    Left ioErr -> do
      hPutStrLn stderr
        ("[tourne] could not read state file at " <> path
         <> " (" <> show ioErr <> "); using empty state")
      pure emptyPersistedState

-- | Write the given state to disk atomically. Best-effort: errors are
-- logged to stderr but never thrown. Returns True on success, False on
-- failure (so callers can choose to log/ignore).
savePersistedState :: PersistedState -> IO Bool
savePersistedState ps = do
  path <- stateFilePath
  result <- tryIOError do
    dir <- getXdgDirectory XdgConfig "tourne"
    createDirectoryIfMissing True dir
    let tmp = path <> ".tmp"
    -- Write to tmp, then atomically rename. If a previous tmp file
    -- exists from a crashed write, the writeFile below overwrites it.
    BSL.writeFile tmp (encodePersistedState ps)
    -- Atomic replace of path with tmp.
    renameFile tmp path
    pure True
  case result of
    Right ok -> pure ok
    Left ioErr -> do
      hPutStrLn stderr
        ("[tourne] could not save state file at " <> path
         <> " (" <> show ioErr <> ")")
      pure False

--------------------------------------------------------------------------------
-- Projection
--------------------------------------------------------------------------------

-- | Project the current AppState into a PersistedState snapshot.
-- We deliberately include only the fields worth restoring; transient
-- runtime fields (event channel, command sink, TVars, ad/failover/
-- ping state, error message) are dropped.
appToPersisted :: AppState -> PersistedState
appToPersisted s = PersistedState
  { psVersion          = currentSchemaVersion
  , psTags             = appTags s
  , psStationsByTag    = appStationsByTag s
  , psCurrentTag       = appCurrentTag s
  , psSelectedStation  = appSelectedStation s
  , psVolume           = appVolume s
  , psFocus            = appFocus s
  , psTagsCursor       = appTagsListState s
  , psStationsCursor   = appStationsListState s
  , psWasPlaying       = appWasPlaying s
  , psResumeStationUrl = resumeUrl
  , psStationSort      = appStationSort s
  }
  where
    -- Cache the URL of the currently selected station so resume can
    -- work even if the stations list isn't loaded yet.
    resumeUrl = case appSelectedStation s of
      Nothing -> Nothing
      Just sid -> fmap stationUrl (lookupStation (appStations s) sid)

