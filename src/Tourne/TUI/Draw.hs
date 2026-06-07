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
import Data.List qualified as List

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
      (txt (" Stations " <> tagSuffix st))
      (viewport StationsListName Vertical $ renderStationsList st)

    content = hBox [tagsPane, stationsPane]
    statusBar = renderStatus st
    helpLine  = renderHelp st
    baseLayout = vBox [content, statusBar, helpLine]

    fullLayout = case appSearchText st of
      Nothing -> baseLayout
      Just searchText ->
        centerLayer $ renderSearch searchText

  in [fullLayout]
  where
    tagSuffix s = case appCurrentTag s of
      Nothing  -> ""
      Just tag -> "[" <> tag <> "]"

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
    stns = appStations st
    focused = appFocus st == FocusStations
    -- Order: stations with known ping first (ascending), then unknown ping by name
    sorted  = sortStationsByPing stns
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
      Just sid  -> fmap stationName $ List.find (\s -> stationId s == sid) (appStations st)

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
    base = " \8593/\8595 Nav  |  Enter Select  |  / Search  |  p Play  |  s Stop  |  q Quit"
    focusHint = case appFocus st of
      FocusTags     -> "  [Tags]"
      FocusStations -> "  [Stations]"
  in
    withAttr helpAttr (txt (base <> focusHint))

--------------------------------------------------------------------------------
-- Search overlay
--------------------------------------------------------------------------------

renderSearch :: Text -> Widget AppName
renderSearch currentText =
  withAttr searchAttr $
    B.borderWithLabel (txt " Search ") $
    hLimit 60 $
    vLimit 3 $
    txt (" " <> currentText <> "|")

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

truncateText :: Int -> Text -> Text
truncateText n t
  | Text.length t > n = Text.take (n - 3) t <> "..."
  | otherwise         = t
