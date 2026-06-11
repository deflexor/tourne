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

import Tourne.Types
  ( AppState (..), Focus (..), ListState (..), Station (..), StationId, Tag
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
  } deriving (Eq, Show, Generic)

instance Aeson.ToJSON PersistedState
instance Aeson.FromJSON PersistedState

-- | Schema version constant. Bump if PersistedState's shape changes.
currentSchemaVersion :: Int
currentSchemaVersion = 1

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
decodePersistedState :: BSL.ByteString -> Either Text PersistedState
decodePersistedState bs = case Aeson.eitherDecode bs of
  Right s
    | psVersion s == currentSchemaVersion -> Right s
    | otherwise -> Left
        ("unsupported schema version: " <> show (psVersion s))
  Left err -> Left (toText err)

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
        ("[tourne] state file at " <> path
         <> " could not be parsed (" <> toString err
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
  }
  where
    -- Cache the URL of the currently selected station so resume can
    -- work even if the stations list isn't loaded yet.
    resumeUrl = case appSelectedStation s of
      Nothing -> Nothing
      Just sid -> fmap stationUrl (find (\st -> stationId st == sid) (appStations s))

