module Tourne.TUI.Events (handleEvent) where

import Relude
import Data.Text qualified as Text
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Control.Concurrent (forkIO)
import Control.Concurrent.STM qualified as STM
import Brick.Main (halt)
import Brick.Types
  ( BrickEvent(..), EventM )
import Brick.BChan (writeBChan)
import Graphics.Vty qualified as Vty

import Tourne.Types
import Tourne.Error (renderError)
import Tourne.RadioBrowser qualified as RB
import Tourne.Persistence
  ( PersistedState (..), appToPersisted, savePersistedState )
import Tourne.TUI.Core (AppName(..))
import Tourne.Audio.Types (AudioCommand(..))

--------------------------------------------------------------------------------
-- Event handler (Brick 2.10 API: returns EventM n s ())
--------------------------------------------------------------------------------

handleEvent :: BrickEvent AppName AppEvent -> EventM AppName AppState ()
handleEvent ev = case ev of

  -------------------------------------------------------------------
  -- Custom events from background threads
  -------------------------------------------------------------------

  AppEvent custom -> case custom of
    EvTick ->
      pure ()

    EvPlayerUpdate playerState ->
      modifySt $ \s -> s{ appPlayerState = playerState }

    EvStreamHealth health ->
      modifySt $ \s -> s{ appStreamHealth = health }

    EvPingUpdate sid result ->
      modifySt $ \s ->
        let updatePing stn
              | stationId stn == sid = case result of
                  Right ping -> stn{ stationPing = Just ping }
                  Left _     -> stn
              | otherwise = stn
            newPings = case result of
              Right ping -> HashMap.insert (unStationId sid) ping (appPingResults s)
              Left _     -> appPingResults s
        in s{ appStations = map updatePing (appStations s)
            , appPingResults = newPings }

    EvAdUpdate adState ->
      modifySt $ \s -> s{ appAdState = adState }

    EvStationsLoaded stations -> do
      -- Auto-resume: if startup-time resume is pending and the
      -- previously selected station is in the freshly loaded list,
      -- fire CmdPlay immediately. Then clear the pending flag so
      -- future EvStationsLoaded (e.g. user navigates to a new tag)
      -- don't re-trigger it.
      st0 <- get
      let shouldResume    = appResumePending st0
          mSelected       = appSelectedStation st0
          mResumeUrl      = psResumeStationUrl (appToPersisted st0)
      modifySt $ \s -> s
        { appStations          = stations
        , appStationsListState = defaultListState
        , appErrorMessage      = Nothing
        , appLoadingStations   = False
        , appStationsByTag     = case appCurrentTag s of
            Nothing -> appStationsByTag s
            Just t  -> HashMap.insert t stations (appStationsByTag s)
        }
      case (shouldResume, mSelected, mResumeUrl) of
        (True, Just sid, Just url) -> case lookupStation stations sid of
          Just stn
            | stationUrl stn == url -> do
                modifySt $ \s -> s
                  { appResumePending = False
                  , appPlayerState   = Connecting (stationUrl stn)
                  }
                sendCmd (CmdPlay (stationUrl stn))
            | otherwise ->
                -- Cached URL doesn't match any loaded station; abort
                -- the resume attempt so we don't re-fire on every load.
                modifySt $ \s -> s{ appResumePending = False }
          Nothing ->
            -- Selected station not in this list (different tag?).
            -- Defer: don't fire resume, leave pending true so the
            -- next EvStationsLoaded (for the right tag) gets a chance.
            pure ()
        _ -> pure ()
      mVar <- gets appStationsVar
      case mVar of
        Just tv -> liftIO $ STM.atomically $ STM.writeTVar tv stations
        Nothing -> pure ()
      schedulePersist

    EvTagsLoaded tags -> do
      modifySt $ \s -> s{ appTags = tags }
      schedulePersist

    EvError err ->
      modifySt $ \s -> s{ appErrorMessage = Just (renderError err)
                        , appPlayerState = ErrorOccurred (renderError err)
                        , appLoadingStations = False
                        }

    EvVolumeUpdate vol -> do
      modifySt $ \s -> s{ appVolume = vol }
      schedulePersist

    EvShutdown -> do
      -- Record the user's "was playing" intent based on current state,
      -- then save. The saved snapshot is what the next launch will
      -- restore from.
      st <- get
      let wasPlaying = case appPlayerState st of
            Playing _      -> True
            Paused         -> True
            Buffering _ _  -> True
            _              -> False
      modifySt $ \s -> s{ appWasPlaying = wasPlaying }
      schedulePersist
      halt

    EvPersistNow -> do
      st <- get
      liftIO $ void $ savePersistedState (appToPersisted st)

  -------------------------------------------------------------------
  -- Vty events (keyboard/mouse)
  -------------------------------------------------------------------

  VtyEvent (Vty.EvMouseDown _ _ _ _) ->
    pure ()

  VtyEvent (Vty.EvKey key _mods) -> case key of

    -- Escape: if the search prompt is open, close it (keep the
    -- committed filter as-is). If no prompt but a filter is active,
    -- clear the filter. Otherwise, quit.
    Vty.KEsc -> do
      st <- get
      case appSearchText st of
        Just _  -> modifySt $ \s -> s{ appSearchText = Nothing }
        Nothing -> case appActiveFilter st of
          Just _  -> modifySt $ \s -> s{ appActiveFilter = Nothing
                                       , appStationsListState = defaultListState
                                       }
          Nothing -> halt

    -- '/' (normal mode): open the search prompt, pre-filled with the
    -- current active filter (so the user can edit the existing filter
    -- or clear it by deleting the text and pressing Enter).
    Vty.KChar '/' -> do
      st <- get
      case appSearchText st of
        Just _  -> pure ()  -- already open; ignore
        Nothing -> do
          let initial = fromMaybe "" (appActiveFilter st)
              initialLen = Text.length initial
          modifySt $ \s -> s{ appSearchText   = Just initial
                            , appSearchCursor = initialLen
                            }

    _ -> do
      inSearch <- gets (isJust . appSearchText)
      if inSearch
        then handleSearchKey key
        else handleNormalKey key

--------------------------------------------------------------------------------
-- Search mode key handling
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Normal mode key handling
--------------------------------------------------------------------------------

handleNormalKey :: Vty.Key -> EventM AppName AppState ()
handleNormalKey key = case key of
  -- Quit
  Vty.KChar 'q' -> halt
  Vty.KChar '\x4' -> halt    -- Ctrl-D

  Vty.KUp    -> navigateVertical (-1)
  Vty.KDown  -> navigateVertical 1
  Vty.KChar 'k' -> navigateVertical (-1)
  Vty.KChar 'j' -> navigateVertical 1

  Vty.KHome -> navigateHome
  Vty.KEnd  -> navigateEnd

  Vty.KPageUp -> navigatePage (-10)
  Vty.KPageDown -> navigatePage 10

  Vty.KChar '\t' -> do
    modifySt $ \s ->
      let newFocus = case appFocus s of
            FocusTags     -> FocusStations
            FocusStations -> FocusTags
      in s{ appFocus = newFocus, appErrorMessage = Nothing }
    schedulePersist

  Vty.KEnter -> handleSelect
  Vty.KChar ' ' -> handleSelect

  -- Play current station
  Vty.KChar 'p' -> do
    mSid <- gets appSelectedStation
    stations <- gets appStations
    case mSid of
      Just sid -> case lookupStation stations sid of
        Just stn -> do
          modifySt $ \s -> s
            { appPlayerState   = Connecting (stationUrl stn)
            , appResumePending = False  -- explicit user action, no auto-resume
            }
          sendCmd (CmdPlay (stationUrl stn))
          schedulePersist
        Nothing -> pure ()
      Nothing -> pure ()

  -- Stop
  Vty.KChar 's' -> do
    modifySt $ \s -> s
      { appPlayerState   = Stopped
      , appSelectedStation = Nothing
      , appResumePending = False  -- user explicitly stopped
      }
    sendCmd CmdStop
    schedulePersist

  -- Volume
  Vty.KChar '+' -> do
    modifySt $ \s -> s{ appVolume = min 1.0 (appVolume s + 0.1) }
    vol <- gets appVolume
    sendCmd (CmdVolume vol)
    schedulePersist
  Vty.KChar '-' -> do
    modifySt $ \s -> s{ appVolume = max 0.0 (appVolume s - 0.1) }
    vol <- gets appVolume
    sendCmd (CmdVolume vol)
    schedulePersist

  -- Cycle station sort mode: Name -> Bitrate -> Ping -> Name.
  -- After changing the mode, try to keep the cursor on the same
  -- station id (so the user's selection doesn't jump away), and
  -- fall back to cursor 0 if the station is no longer present.
  Vty.KChar 'o' -> do
    st <- get
    let prevSelected = appSelectedStation st
        newMode      = nextSortMode (appStationSort st)
        sorted       = sortStations newMode (appStations st)
        -- Find the new index of the previously selected station, if any.
        newIdx = case prevSelected of
          Nothing -> 0
          Just sid -> case List.findIndex (\s -> stationId s == sid) sorted of
            Just i  -> i
            Nothing -> 0
    modifySt $ \s -> s
      { appStationSort       = newMode
      , appStationsListState = (appStationsListState s)
          { listSelected = newIdx
          , listOffset   = min (listOffset (appStationsListState s)) newIdx
          }
      }
    schedulePersist

  -- Refresh
  Vty.KChar 'r' -> pure ()

  _ -> pure ()

--------------------------------------------------------------------------------
-- Selection handler
--------------------------------------------------------------------------------

-- | Send a command to the audio engine (if available)
sendCmd :: AudioCommand -> EventM AppName AppState ()
sendCmd cmd = do
  mFn <- gets appAudioCommand
  case mFn of
    Just fn -> liftIO $ fn cmd
    Nothing -> pure ()

handleSelect :: EventM AppName AppState ()
handleSelect = do
  focus <- gets appFocus
  case focus of
    FocusTags -> do
      tags <- gets appTags
      idx  <- gets (listSelected . appTagsListState)
      if idx >= 0 && idx < length tags
        then do
          let selectedTagName = tagName (List.genericIndex tags idx)
          -- Show immediate feedback: clear error, show loading state
          modifySt $ \s -> s{ appCurrentTag = Just selectedTagName
                            , appErrorMessage = Nothing
                            , appLoadingStations = True
                            , appResumePending = False  -- user moved on
                            }
          -- Fetch stations for the selected tag in background
          mChan <- gets appEventChan
          cfg   <- gets appConfig
          case mChan of
            Just chan -> liftIO $ void $ forkIO $ do
              result <- RB.fetchStationsByTag cfg selectedTagName 100
              case result of
                Right stations -> writeBChan chan (EvStationsLoaded stations)
                Left err       -> writeBChan chan (EvError err)
            Nothing -> pure ()
          schedulePersist
        else pure ()

    FocusStations -> do
      stations <- gets appStations
      sortMode <- gets appStationSort
      idx      <- gets (listSelected . appStationsListState)
      -- Order must match the display (see Draw.hs renderStationsList)
      let sorted = sortStations sortMode stations
      if idx >= 0 && idx < length sorted
        then do
          let station = List.genericIndex sorted idx
          modifySt $ \s -> s
            { appSelectedStation = Just (stationId station)
            , appPlayerState = Connecting (stationUrl station)
            , appResumePending = False  -- explicit user action
            }
          sendCmd (CmdPlay (stationUrl station))
          schedulePersist
        else pure ()

--------------------------------------------------------------------------------
-- Navigation helpers
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Update scroll offset to keep the selected item visible
updateOffset :: Int -> Int -> Int -> Int
updateOffset offset selected visible
  | selected < offset              = selected
  | selected >= offset + visible   = selected - visible + 1
  | otherwise                      = offset

clamp :: Int -> Int -> Int -> Int
clamp lo hi x
  | x < lo    = lo
  | x > hi    = hi
  | otherwise = x

-- | State modification helper (Relude doesn't export modify')
modifySt :: MonadState s m => (s -> s) -> m ()
modifySt f = get >>= put . f

-- | Check if a character is printable
isPrint :: Char -> Bool
isPrint c = c >= ' ' && c <= '~'

--------------------------------------------------------------------------------
-- Persistence
--------------------------------------------------------------------------------

-- | Trigger an EvPersistNow via the event channel so a save happens
-- shortly. Falls back to a direct save if the channel isn't wired up
-- (which would only happen in misconfigured test setups).
schedulePersist :: EventM AppName AppState ()
schedulePersist = do
  mChan <- gets appEventChan
  case mChan of
    Just chan -> liftIO $ void $ writeBChan chan EvPersistNow
    Nothing   -> do
      st <- get
      liftIO $ void $ savePersistedState (appToPersisted st)
