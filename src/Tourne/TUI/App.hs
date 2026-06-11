module Tourne.TUI.App
  ( initialAppState
  , app
  ) where

import Relude
import Data.HashMap.Strict qualified as HashMap
import Brick.Main
  ( App(..), neverShowCursor )
import Brick.AttrMap (AttrMap, attrMap)
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes (defAttr, withForeColor, withBackColor)

import Tourne.Types
import Tourne.Persistence (PersistedState (..))
import Tourne.TUI.Core
import Tourne.TUI.Draw (drawUI)
import Tourne.TUI.Events (handleEvent)
import Brick.BChan (BChan)

--------------------------------------------------------------------------------
-- Initial app state
--------------------------------------------------------------------------------

-- | Build the initial AppState. Tags, stations, and navigation state
-- are seeded from the persisted snapshot (if any) so the user sees
-- the same view they left on the previous run. Runtime-only fields
-- (event channel, command sink, TVars) are left as their default
-- 'Nothing' values and populated by 'Main' before the app launches.
initialAppState :: Maybe (BChan AppEvent) -> PersistedState -> IO AppState
initialAppState mChan persisted = pure AppState
  { appTags              = psTags persisted
  , appStations          = initialStations persisted
  , appCurrentTag        = psCurrentTag persisted
  , appSelectedStation   = psSelectedStation persisted
  , appPlayerState       = Stopped
  , appFocus             = psFocus persisted
  , appSearchText        = Nothing
  , appSearchCursor      = 0
  , appActiveFilter      = Nothing
  , appAdState           = AdInactive
  , appFailoverState     = FailoverInactive
  , appStreamHealth      = StreamGood
  , appErrorMessage      = Nothing
  , appPingResults       = mempty
  , appVolume            = psVolume persisted
  , appTagsListState     = psTagsCursor persisted
  , appStationsListState = psStationsCursor persisted
  , appEventChan         = mChan
  , appLoadingStations   = False
  , appAudioCommand      = Nothing
  , appStationsVar       = Nothing
  , appWasPlaying        = psWasPlaying persisted
  , appResumePending     = psWasPlaying persisted
  , appStationsByTag     = psStationsByTag persisted
  }

-- | If we have a cached station list for the last selected tag, use
-- it as the initial stations list so the user sees the same stations
-- immediately. Otherwise leave empty (the user will need to pick a
-- tag, or Main.hs will trigger a background refresh).
initialStations :: PersistedState -> [Station]
initialStations ps = case psCurrentTag ps of
  Nothing -> []
  Just tag -> fromMaybe [] (HashMap.lookup tag (psStationsByTag ps))

--------------------------------------------------------------------------------
-- Brick App definition
--------------------------------------------------------------------------------

app :: App AppState AppEvent AppName
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const theAttrMap
  }

--------------------------------------------------------------------------------
-- Attribute map
--
-- Colors accessed via Vty.qualified module (Graphics.Vty re-exports all)
-- Attribute combining via Vty.withForeColor / withBackColor
--------------------------------------------------------------------------------

fgWhiteOnBlue :: Vty.Attr
fgWhiteOnBlue = withBackColor (withForeColor defAttr Vty.white) Vty.blue

theAttrMap :: AttrMap
theAttrMap = attrMap Vty.defAttr
  [ (tagSelectedAttr,          fgWhiteOnBlue)
  , (stationSelectedAttr,      fgWhiteOnBlue)
  , (statusAttr,               withForeColor defAttr Vty.green)
  , (errorAttr,                withForeColor defAttr Vty.red)
  , (helpAttr,                 withForeColor defAttr Vty.yellow)
  , (playingAttr,              withForeColor defAttr Vty.green)
  , (searchAttr,               withForeColor defAttr Vty.white)
  , (titleAttr,                withForeColor defAttr Vty.white)
  ]
