{-|
Module      : Tourne.TUI.Events.Normal
Description : Key handling when no modal is open.

This is the 'default' keymap: navigation, selection, transport
(play/stop), volume, sort mode, and quit. 'handleSelect' is
defined here (not in Dispatch) because it shares the
normal-mode use site.
-}
module Tourne.TUI.Events.Normal
  ( handleNormalKey
  , handleSelect
  , sendCmd
  ) where

import Relude
import Data.List qualified as List
import Control.Concurrent (forkIO)
import Brick.Main (halt)
import Brick.Types (EventM, get, gets)
import Brick.BChan (writeBChan)
import Graphics.Vty qualified as Vty

import Tourne.Types
import Tourne.Audio.Types (AudioCommand (..))
import Tourne.RadioBrowser qualified as RB
import Tourne.TUI.Core (AppName)
import Tourne.TUI.Events.Navigate
  ( navigateVertical, navigateHome, navigateEnd, navigatePage )
import Tourne.TUI.Events.Persist (schedulePersist)
import Tourne.TUI.Events.Util (modifySt)

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
