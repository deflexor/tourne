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
        (True, Just sid, Just url) -> case find (\st -> stationId st == sid) stations of
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
      modifySt $ \s -> s{ appErrorMessage = Just err
                        , appPlayerState = ErrorOccurred err
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

    -- Quit on Escape (when not searching)
    Vty.KEsc -> do
      inSearch <- gets (isJust . appSearchText)
      if inSearch
        then modifySt $ \s -> s{ appSearchText = Nothing }
        else halt

    -- Search mode: '/' toggles search
    Vty.KChar '/' ->
      modifySt $ \s -> s{ appSearchText = Just (fromMaybe "" (appSearchText s)) }

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
  Vty.KEsc ->
    modifySt $ \s -> s{ appSearchText = Nothing }

  Vty.KEnter ->
    pure ()

  Vty.KBS ->
    modifySt $ \s ->
      let current = fromMaybe "" (appSearchText s)
          newText = Text.dropEnd 1 current
      in s{ appSearchText = Just newText }

  Vty.KChar c
    | isPrint c ->
        modifySt $ \s ->
          let current = fromMaybe "" (appSearchText s)
              newText = current <> Text.singleton c
              filtered = filterStations (appStations s) newText
          in s{ appSearchText = Just newText, appStations = filtered }
    | otherwise -> pure ()

  _ -> pure ()

filterStations :: [Station] -> Text -> [Station]
filterStations stations query
  | Text.null query = stations
  | otherwise =
    let lowerQuery = Text.toLower query
    in filter (\s -> lowerQuery `Text.isInfixOf` Text.toLower (stationName s)) stations

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
      Just sid -> case find (\s -> stationId s == sid) stations of
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
          case mChan of
            Just chan -> liftIO $ void $ forkIO $ do
              result <- RB.fetchStationsByTag selectedTagName 100
              case result of
                Right stations -> writeBChan chan (EvStationsLoaded stations)
                Left err       -> writeBChan chan (EvError (toText err))
            Nothing -> pure ()
          schedulePersist
        else pure ()

    FocusStations -> do
      stations <- gets appStations
      idx      <- gets (listSelected . appStationsListState)
      -- Order must match the display (see Draw.hs renderStationsList)
      let sorted = sortStationsByPing stations
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
