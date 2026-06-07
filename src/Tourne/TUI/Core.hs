module Tourne.TUI.Core
  ( AppName(..)
  , tagSelectedAttr, stationSelectedAttr, statusAttr, errorAttr
  , helpAttr, playingAttr, searchAttr, titleAttr
  ) where

import Relude
import Brick.AttrMap qualified as A

--------------------------------------------------------------------------------
-- Widget names for focus/identification
--------------------------------------------------------------------------------

data AppName
  = TagsListName
  | StationsListName
  | StatusBarName
  | SearchBoxName
  | MainWindow
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Attribute names for styling
--------------------------------------------------------------------------------

tagSelectedAttr, stationSelectedAttr, statusAttr, errorAttr :: A.AttrName
helpAttr, playingAttr, searchAttr, titleAttr :: A.AttrName
tagSelectedAttr     = A.attrName "tagSelected"
stationSelectedAttr = A.attrName "stationSelected"
statusAttr          = A.attrName "status"
errorAttr           = A.attrName "error"
helpAttr            = A.attrName "help"
playingAttr         = A.attrName "playing"
searchAttr          = A.attrName "search"
titleAttr           = A.attrName "title"
