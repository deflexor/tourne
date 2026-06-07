module Tourne.TUI.App
  ( initialAppState
  , app
  ) where

import Relude
import Brick.Main
  ( App(..), neverShowCursor, halt )
import Brick.Types
  ( Widget, EventM, ViewportType(..), BrickEvent(..) )
import Brick.AttrMap (AttrMap, attrMap)
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes (defAttr, withForeColor, withBackColor)

import Tourne.Types
import Tourne.Config
import Tourne.TUI.Core
import Tourne.TUI.Draw (drawUI)
import Tourne.TUI.Events (handleEvent)
import Brick.BChan (BChan)

--------------------------------------------------------------------------------
-- Initial app state
--------------------------------------------------------------------------------

initialAppState :: Config -> Maybe (BChan AppEvent) -> IO AppState
initialAppState cfg mChan = pure AppState
  { appTags             = []
  , appStations         = []
  , appCurrentTag       = Nothing
  , appSelectedStation  = Nothing
  , appPlayerState      = Stopped
  , appFocus            = FocusTags
  , appSearchText       = Nothing
  , appAdState          = AdInactive
  , appFailoverState    = FailoverInactive
  , appStreamHealth     = StreamGood
  , appErrorMessage     = Nothing
  , appPingResults      = mempty
  , appVolume           = configPlayerVolume cfg
  , appTagsListState    = defaultListState
  , appStationsListState = defaultListState
  , appEventChan        = mChan
  , appLoadingStations  = False
  , appAudioCommand     = Nothing
  , appStationsVar      = Nothing
  }

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
