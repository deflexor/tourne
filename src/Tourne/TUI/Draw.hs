module Tourne.TUI.Draw (drawUI) where

import Relude
import Data.Text qualified as Text
import Brick.Types
  ( Widget, ViewportType(..) )
import Brick.Widgets.Core
  ( str, txt, (<+>), (<=>), withAttr, vLimit, hLimit
  , viewport, vBox, hBox
  )
import Brick.Widgets.Center (centerLayer)
import Brick.Widgets.Border qualified as B

import Tourne.Types
import Tourne.TUI.Core
  ( AppName(..)
  , tagSelectedAttr, stationSelectedAttr, statusAttr, errorAttr
  , helpAttr, playingAttr, searchAttr, titleAttr
  )

--------------------------------------------------------------------------------
-- Main draw function
--------------------------------------------------------------------------------

drawUI :: AppState -> [Widget AppName]
drawUI st =
  let
    tagsPane = B.borderWithLabel (txt " Tags ") $
      hLimit 28 $ vLimit 50 $ viewport TagsListName Vertical $
        renderTagsList st

    stationsPane = B.borderWithLabel
      (txt (" Stations " <> stationsHeaderSuffix st))
      (viewport StationsListName Vertical $ renderStationsList st)

    content = hBox [tagsPane, stationsPane]
    statusBar = renderStatus st
    helpLine  = renderHelp st
    baseLayout = vBox [content, statusBar, helpLine]

    -- The search prompt is a single-line full-width banner at the top
    -- of the screen. It occupies one row when open; the base layout is
    -- pushed down by one row, but the prompt spans the entire terminal
    -- width so there are no side gutters. Pressing Esc closes it and
    -- restores the layout.
    fullLayout = case appSearchText st of
      Nothing    -> baseLayout
      Just query -> vBox
        [ renderSearchPrompt query (appSearchCursor st)
        , baseLayout
        ]

  in [fullLayout]
  where
    -- Suffix for the Stations pane title: includes the current tag,
    -- (if set) the active filter, and the active sort mode. Examples:
    --   ""                                  — no tag, no filter, no sort
    --   "[jazz]"                            — tag only
    --   "(filter: 'foo')"                   — filter only
    --   "[jazz] (sort: name)"               — tag + sort
    --   "[jazz] (filter: 'foo') (sort: bitrate)"
    stationsHeaderSuffix s = tagSuffix s <> filterSuffix s <> sortSuffix s
    tagSuffix s = case appCurrentTag s of
      Nothing  -> ""
      Just tag -> "[" <> tag <> "]"
    filterSuffix s = case appActiveFilter s of
      Nothing -> ""
      Just f  -> " (filter: '" <> f <> "')"
    sortSuffix s = " (sort: " <> sortModeLabel (appStationSort s) <> ")"

--------------------------------------------------------------------------------
-- Tags list
--------------------------------------------------------------------------------

renderTagsList :: AppState -> Widget AppName
renderTagsList st =
  let
    sel  = listSelected (appTagsListState st)
    off  = listOffset (appTagsListState st)
    tags = appTags st
    focused = appFocus st == FocusTags
    visible = drop off tags

    renderTag tag idx =
      let
        name  = tagName tag
        count = show (tagStationCount tag)
        prefix = if idx == sel then " > " else "   "
        line = prefix <> name <> " (" <> count <> ")"
      in
        if idx == sel && focused
        then withAttr tagSelectedAttr (txt line)
        else txt line
  in
    if null tags
    then txt " Loading tags..."
    else vBox (zipWith renderTag visible [off..])

--------------------------------------------------------------------------------
-- Stations list
--------------------------------------------------------------------------------

renderStationsList :: AppState -> Widget AppName
renderStationsList st =
  let
    sel  = listSelected (appStationsListState st)
    off  = listOffset (appStationsListState st)
    stns = viewStations st
    focused = appFocus st == FocusStations
    -- Order: determined by the user-selected sort mode (Name /
    -- Bitrate / Ping). The list is no longer re-sorted implicitly
    -- on every ping update; pings stream in as data and are
    -- reflected in the per-row ping column, but row order is
    -- stable until the user toggles it with the 'o' key.
    sorted  = sortStations (appStationSort st) stns
    visible = drop off sorted

    -- Column widths (characters)
    nameWidth  = 30
    codecWidth = 15
    pingWidth  = 7

    renderStation station idx =
      let
        name    = truncateText nameWidth (stationName station)
        br      = maybe "-" show (stationBitrate station)
        pingStr = case stationPing station of
          Nothing -> "    ?ms"
          Just p  -> Text.justifyRight pingWidth ' '
                       (show (round (p * 1000) :: Int) <> "ms")
        codec   = fromMaybe "-" (stationCodec station)
        prefix  = if idx == sel then ">" else " "
        playing = appSelectedStation st == Just (stationId station)
        marker  = if playing then "\9835 " else "  "
        -- Table columns
        nameCol  = Text.justifyLeft nameWidth ' ' name
        codecCol = Text.justifyRight codecWidth ' '
                     ("[" <> codec <> "/" <> br <> "kbps]")
        line = prefix <> marker <> " " <> nameCol <> "  " <> codecCol <> "  " <> pingStr
      in
        if idx == sel && focused
        then withAttr stationSelectedAttr (txt line)
        else if playing
             then withAttr playingAttr (txt line)
             else txt line
  in
    if null stns
    then txt " Select a tag to see stations\n\n Press '/' to search"
    else vBox (zipWith renderStation visible [off..])

--------------------------------------------------------------------------------
-- Status bar
--------------------------------------------------------------------------------

renderStatus :: AppState -> Widget AppName
renderStatus st =
  let
    -- Error message takes priority over everything
    errMsg = appErrorMessage st
    isLoading = appLoadingStations st

    -- Look up currently playing station name
    playingName = case appSelectedStation st of
      Nothing   -> Nothing
      Just sid  -> fmap stationName $ lookupStation (appStations st) sid

    stateStr = case errMsg of
      Just e  -> " Error: " <> e
      Nothing
        | isLoading -> " Loading stations..."
        | otherwise -> case appPlayerState st of
            Stopped          -> " Stopped"
            Connecting url   -> " Connecting: " <> truncateText 50 url
            Buffering cur total -> " Buffering " <> progressBar cur total
            Playing _        -> " Playing: " <> fromMaybe "?" playingName <> volBar (appVolume st)
            Paused           -> " Paused"
            ErrorOccurred e  -> " Error: " <> e

    healthStr = case (errMsg, appStreamHealth st) of
      (Just _, _)   -> ""
      (Nothing, StreamGood)       -> ""
      (Nothing, StreamDegraded d) -> " [Degraded: " <> d <> "]"
      (Nothing, StreamLost l)     -> " [Lost: " <> l <> "]"

    full = stateStr <> healthStr
  in
    case errMsg of
      Just _  -> withAttr errorAttr (txt full)
      Nothing -> case appPlayerState st of
        ErrorOccurred _ -> withAttr errorAttr (txt full)
        Playing _       -> withAttr playingAttr (txt full)
        _               -> withAttr statusAttr (txt full)

volBar :: Double -> Text
volBar vol =
  let n = round (vol * 10) :: Int
      bars = Text.replicate n "|"
      dots = Text.replicate (10 - n) "."
  in " [" <> bars <> dots <> "]"

progressBar :: Int -> Int -> Text
progressBar cur total
  | total <= 0 = "..."
  | otherwise =
    let width = 10
        filled = (cur * width) `div` max total 1
        bars     = Text.replicate filled "#"
        empties  = Text.replicate (width - filled) "."
        kbCur    = cur `div` 1024
        kbTotal  = total `div` 1024
    in "[" <> bars <> empties <> "] " <> show kbCur <> "/" <> show kbTotal <> " KB"

--------------------------------------------------------------------------------
-- Help line
--------------------------------------------------------------------------------

renderHelp :: AppState -> Widget AppName
renderHelp st =
  let
    base = " \8593/\8595 Nav  |  Enter Select  |  / Search  |  p Play  |  s Stop  |  o Sort  |  q Quit"
    focusHint = case appFocus st of
      FocusTags     -> "  [Tags]"
      FocusStations -> "  [Stations]"
    filterHint = case appActiveFilter st of
      Nothing -> ""
      Just _  -> "  [filter: Esc to clear]"
  in
    withAttr helpAttr (txt (base <> focusHint <> filterHint))

--------------------------------------------------------------------------------
-- Search prompt (full-width single-line banner at the top of the screen)
--------------------------------------------------------------------------------

-- | Render the search prompt as a single-line full-width bordered widget
-- with the 'Search' label and a textual cursor at the given position.
-- The cursor is rendered as a '|' character between the text's left
-- and right halves; the layout is intentionally simple — brick's
-- border widget sizes to its contents, and we omit width limits so the
-- border stretches to the full terminal width.
renderSearchPrompt :: Text -> Int -> Widget AppName
renderSearchPrompt currentText cursor =
  withAttr searchAttr $
    B.borderWithLabel (txt " Search ") $
    txt (insertCursor currentText cursor)

-- | Insert a visible '|' cursor character at position @cursor@ in @t@.
-- Cursor is clamped to @[0, Text.length t]@. An empty @t@ with cursor 0
-- yields just "|" so the user always sees where they'll type.
insertCursor :: Text -> Int -> Text
insertCursor t cursor =
  let len = Text.length t
      pos = clamp 0 len cursor
      (before, after) = Text.splitAt pos t
  in before <> Text.singleton '|' <> after

-- | Clamp @x@ to the inclusive range @[lo, hi]@. Local helper; mirrors
-- the one in 'Tourne.TUI.Events' but is duplicated here so each module
-- stays self-contained (small, pure, no need to share).
clamp :: Int -> Int -> Int -> Int
clamp lo hi x
  | x < lo    = lo
  | x > hi    = hi
  | otherwise = x

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

truncateText :: Int -> Text -> Text
truncateText n t
  | Text.length t > n = Text.take (n - 3) t <> "..."
  | otherwise         = t

--------------------------------------------------------------------------------
-- Station view (unfiltered or search-filtered)
--------------------------------------------------------------------------------

-- | The list of stations to display. Filter resolution:
--   * If the search prompt is open (@appSearchText = Just q@), filter by
--     @q@ — the user sees a live preview as they type.
--   * Else if a filter is committed (@appActiveFilter = Just f@), filter
--     by @f@ — the list stays filtered after the prompt is closed.
--   * Else return the full unfiltered list.
--
-- The unfiltered source-of-truth (@appStations@) is never mutated by the
-- search; this is a pure view, computed per render.
viewStations :: AppState -> [Station]
viewStations st = case appSearchText st <|> appActiveFilter st of
  Nothing -> appStations st
  Just q  -> filterStations (appStations st) q

-- | Pure filter: case-insensitive substring match on station name.
-- Empty query is a no-op (returns the input unchanged).
filterStations :: [Station] -> Text -> [Station]
filterStations stations query
  | Text.null query = stations
  | otherwise =
    let lowerQuery = Text.toLower query
    in filter (\s -> lowerQuery `Text.isInfixOf` Text.toLower (stationName s)) stations
