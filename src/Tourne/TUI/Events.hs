module Tourne.TUI.Events (handleEvent) where

import Relude
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Brick.Main (halt)
import Brick.Types (BrickEvent (..), EventM, get, gets)
import Graphics.Vty qualified as Vty
import Control.Concurrent.STM qualified as STM

import Tourne.Types
import Tourne.Error (renderError)
import Tourne.TUI.Core (AppName)
import Tourne.TUI.Events.Persist (schedulePersist)
import Tourne.TUI.Events.Util (modifySt)
import Tourne.TUI.Events.Search (handleSearchKey)
import Tourne.TUI.Events.Normal (handleNormalKey, sendCmd)
import Tourne.Persistence
  ( appToPersisted, savePersistedState, psResumeStationUrl )
import Tourne.Audio.Types (AudioCommand (..))

-- | Top-level Brick event dispatcher. Routes an incoming event to
-- either an AppEvent-specific update, a Vty key handler (after a
-- modal-mode check), or drops it.
handleEvent :: BrickEvent AppName AppEvent -> EventM AppName AppState ()
handleEvent ev = case ev of

  -------------------------------------------------------------------
  -- Custom events from background threads
  -------------------------------------------------------------------

  AppEvent custom -> case custom of
    EvTick -> do
      -- Advance the now-playing scroll by 1 character. The render
      -- function in Draw.hs concatenates the title with itself so
      -- a single integer offset is enough for seamless wrap-around.
      -- Modulo is taken inside the render to keep the state value
      -- small.
      modifySt $ \s -> s{ appNowPlayingScroll = appNowPlayingScroll s + 1 }

    EvPlayerUpdate playerState ->
      modifySt $ \s -> s{ appPlayerState = playerState }

    EvStreamHealth health ->
      modifySt $ \s -> s{ appStreamHealth = health }

    EvIcyMetaUpdate title -> do
      -- Reset the scroll on every new title. Without this, a
      -- 30-character title starting at scroll 25 would briefly
      -- display the end of the previous song when a new song
      -- starts.
      modifySt $ \s -> s{ appIcyMetadata      = title
                         , appNowPlayingScroll = 0
                         }

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
