{-|
Module      : Tourne.TUI.Events.Navigate
Description : Cursor / scroll-window navigation across the two list panes.

'visibleCount' is the number of items the renderer shows in
each pane; @updateOffset@ shifts the scroll window so the
selected index stays visible.

Each navigation primitive (up/down, Home/End, PageUp/PageDown)
clears the error message and triggers 'schedulePersist'.
-}
module Tourne.TUI.Events.Navigate
  ( visibleCount
  , navigateVertical
  , navigateHome
  , navigateEnd
  , navigatePage
  , updateOffset
  ) where

import Relude
import Brick.Types (EventM, get, gets)

import Tourne.Types
import Tourne.TUI.Core (AppName)
import Tourne.TUI.Events.Util (clamp, modifySt)
import Tourne.TUI.Events.Persist (schedulePersist)

-- | Number of visible items per pane (for scroll windowing)
visibleCount :: Int
visibleCount = 15

navigateVertical :: Int -> EventM AppName AppState ()
navigateVertical delta = do
  focus <- gets appFocus
  -- Clear error message on any navigation
  modifySt $ \s -> s{ appErrorMessage = Nothing }
  case focus of
    FocusTags -> do
      listState <- gets appTagsListState
      tags      <- gets appTags
      let maxIdx = max 0 (length tags - 1)
          newIdx = clamp 0 maxIdx (listSelected listState + delta)
          newOffset = updateOffset (listOffset listState) newIdx visibleCount
      modifySt $ \s -> s{ appTagsListState = listState{ listSelected = newIdx, listOffset = newOffset } }

    FocusStations -> do
      listState <- gets appStationsListState
      stations  <- gets appStations
      let maxIdx = max 0 (length stations - 1)
          newIdx = clamp 0 maxIdx (listSelected listState + delta)
          newOffset = updateOffset (listOffset listState) newIdx visibleCount
      modifySt $ \s -> s{ appStationsListState = listState{ listSelected = newIdx, listOffset = newOffset } }
  schedulePersist

navigateHome :: EventM AppName AppState ()
navigateHome = do
  focus <- gets appFocus
  case focus of
    FocusTags     -> modifySt $ \s -> s{ appTagsListState = (appTagsListState s){ listSelected = 0, listOffset = 0 } }
    FocusStations -> modifySt $ \s -> s{ appStationsListState = (appStationsListState s){ listSelected = 0, listOffset = 0 } }
  schedulePersist

navigateEnd :: EventM AppName AppState ()
navigateEnd = do
  focus <- gets appFocus
  case focus of
    FocusTags ->
      modifySt $ \s ->
        let mx = max 0 (length (appTags s) - 1)
            off = max 0 (mx - visibleCount + 1)
        in s{ appTagsListState = (appTagsListState s){ listSelected = mx, listOffset = off } }
    FocusStations ->
      modifySt $ \s ->
        let mx = max 0 (length (appStations s) - 1)
            off = max 0 (mx - visibleCount + 1)
        in s{ appStationsListState = (appStationsListState s){ listSelected = mx, listOffset = off } }
  schedulePersist

navigatePage :: Int -> EventM AppName AppState ()
navigatePage delta = do
  focus <- gets appFocus
  case focus of
    FocusTags -> do
      listState <- gets appTagsListState
      tags      <- gets appTags
      let maxIdx = max 0 (length tags - 1)
          newIdx = clamp 0 maxIdx (listSelected listState + delta)
          newOffset = updateOffset (listOffset listState) newIdx visibleCount
      modifySt $ \s -> s{ appTagsListState = listState{ listSelected = newIdx, listOffset = newOffset } }
    FocusStations -> do
      listState <- gets appStationsListState
      stations  <- gets appStations
      let maxIdx = max 0 (length stations - 1)
          newIdx = clamp 0 maxIdx (listSelected listState + delta)
          newOffset = updateOffset (listOffset listState) newIdx visibleCount
      modifySt $ \s -> s{ appStationsListState = listState{ listSelected = newIdx, listOffset = newOffset } }
  schedulePersist

-- | Update scroll offset to keep the selected item visible.
updateOffset :: Int -> Int -> Int -> Int
updateOffset offset selected visible
  | selected < offset              = selected
  | selected >= offset + visible   = selected - visible + 1
  | otherwise                      = offset
