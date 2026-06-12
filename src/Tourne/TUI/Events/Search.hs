{-|
Module      : Tourne.TUI.Events.Search
Description : Key handling for the search-prompt modal.

'Search' = the user has pressed '/' and is typing a filter
expression. The mode is closed by 'Vty.KEsc' (filter preserved)
or 'Vty.KEnter' (filter committed; empty text clears it).
-}
module Tourne.TUI.Events.Search
  ( handleSearchKey
  , moveCursor
  ) where

import Relude
import Data.Text qualified as Text
import Graphics.Vty qualified as Vty
import Brick.Types (EventM, get)

import Tourne.Types
import Tourne.TUI.Core (AppName)
import Tourne.TUI.Events.Navigate (navigateVertical)
import Tourne.TUI.Events.Util (clamp, isPrint, modifySt)

handleSearchKey :: Vty.Key -> EventM AppName AppState ()
handleSearchKey key = case key of

  -- Close the prompt; the committed filter is left unchanged.
  Vty.KEsc ->
    modifySt $ \s -> s{ appSearchText   = Nothing
                      , appSearchCursor = 0
                      }

  -- Commit the prompt's text to the active filter and close the prompt.
  -- Empty text means "clear the filter".
  Vty.KEnter -> do
    st <- get
    let typed = fromMaybe "" (appSearchText st)
    modifySt $ \s -> s
      { appSearchText        = Nothing
      , appSearchCursor      = 0
      , appActiveFilter      = if Text.null typed then Nothing else Just typed
      , appStationsListState = defaultListState
      }

  -- Move the text cursor left/right.
  Vty.KLeft  -> moveCursor (-1)
  Vty.KRight -> moveCursor 1

  -- Up/Down/j/k: navigate the (filtered) stations list. The list is
  -- already filtered live by Draw.viewStations, so the normal navigation
  -- helpers Just Work against the filtered view.
  Vty.KUp       -> navigateVertical (-1)
  Vty.KDown     -> navigateVertical 1
  Vty.KChar 'k' -> navigateVertical (-1)
  Vty.KChar 'j' -> navigateVertical 1

  -- Backspace: delete the character before the cursor. If the cursor
  -- is at position 0, this is a no-op.
  Vty.KBS -> do
    st <- get
    let typed   = fromMaybe "" (appSearchText st)
        cursor  = appSearchCursor st
    if cursor <= 0
      then pure ()
      else let (before, after) = Text.splitAt cursor typed
               -- drop the character immediately before the cursor
               newText  = Text.dropEnd 1 before <> after
               newCur   = cursor - 1
           in modifySt $ \s -> s{ appSearchText   = Just newText
                                 , appSearchCursor = newCur
                                 }

  -- Printable characters: insert at the cursor position.
  Vty.KChar c
    | isPrint c -> do
        st <- get
        let typed   = fromMaybe "" (appSearchText st)
            cursor  = appSearchCursor st
            (before, after) = Text.splitAt cursor typed
            newText  = before <> Text.singleton c <> after
            newCur   = cursor + 1
        modifySt $ \s -> s{ appSearchText   = Just newText
                          , appSearchCursor = newCur
                          }
    | otherwise -> pure ()

  _ -> pure ()

-- | Move the search-prompt text cursor by @delta@, clamped to
-- @[0, Text.length query]@. No-op if the prompt is closed.
moveCursor :: Int -> EventM AppName AppState ()
moveCursor delta = do
  st <- get
  case appSearchText st of
    Nothing -> pure ()
    Just q  ->
      let len    = Text.length q
          newCur = clamp 0 len (appSearchCursor st + delta)
      in modifySt $ \s -> s{ appSearchCursor = newCur }
