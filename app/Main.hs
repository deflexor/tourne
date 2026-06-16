module Main (main) where

import Relude
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM qualified as STM
import System.IO (hPutStrLn)
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Main (customMain)
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty qualified as Vty
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Posix.Signals (sigINT, sigTERM, installHandler, Handler(Catch))

import Tourne.Types
import Tourne.Error (renderError)
import Tourne.Config
import Tourne.RadioBrowser qualified as RB
import Tourne.AdDetection qualified as AD
import Tourne.Audio.Player qualified as Audio
import Tourne.PingChecker (startPingChecker, stopPingChecker)
import Tourne.Persistence
  ( PersistedState (..), loadPersistedState, savePersistedState
  , appToPersisted
  )
import Tourne.TUI.App qualified as TUI

--------------------------------------------------------------------------------
-- Main entry point
--------------------------------------------------------------------------------

main :: IO ()
main = do
  let cfg = defaultConfig
  chan <- newBChan 100

  -- Build the shared HTTP Manager once for the whole program.
  -- Used by the radio-browser API client, the audio stream
  -- fetcher, and the ping checker; sharing it keeps the
  -- connection pool warm.
  httpMgr <- newManager tlsManagerSettings

  -- Initialize audio engine
  audioEngine <- Audio.initAudio httpMgr >>= \case
    Right e  -> pure e
    Left err -> do
      hPutStrLn stderr ("Audio init failed: " <> toString (renderError err))
      exitFailure

  -- Initialize ad detector
  _adDetector <- AD.initAdDetector

  -- Load persisted state (tags, last selected tag/station, volume,
  -- focus, cursors). On any read failure this falls back to an
  -- empty state with a notice on stderr.
  persisted <- loadPersistedState

  -- Always kick a background tag refresh so the list stays current,
  -- but the UI shows the cached tags immediately on the first frame.
  _ <- forkIO $ do
    result <- RB.fetchTags httpMgr cfg
    case result of
      Right tags -> writeBChan chan (EvTagsLoaded tags)
      Left err   -> writeBChan chan (EvError err)

  -- Always kick a background station refresh for the last selected
  -- tag, in case the cached list is stale. EvStationsLoaded will
  -- trigger the auto-resume if appropriate.
  case psCurrentTag persisted of
    Just tag -> void $ forkIO $ do
      result <- RB.fetchStationsByTag httpMgr cfg tag (configMaxStations cfg)
      case result of
        Right stations -> writeBChan chan (EvStationsLoaded stations)
        Left _         -> pure ()  -- keep cached list on failure
    Nothing -> pure ()

  -- Install signal handlers for graceful shutdown
  _ <- installHandler sigINT  (Catch (writeBChan chan EvShutdown)) Nothing
  _ <- installHandler sigTERM (Catch (writeBChan chan EvShutdown)) Nothing

  -- Create stations TVar for PingChecker
  stationsVar <- STM.newTVarIO []

  -- Start background ping checker. Pings are issued in batches of
  -- configPingBatchSize stations every configPingIntervalSeconds seconds,
  -- with a per-request response timeout of configPingResponseTimeout
  -- seconds. Results are routed through the Brick event channel as
  -- EvPingUpdate, which the TUI handler turns into appPingResults and
  -- per-station stationPing values (used by sorting and failover).
  pingHandle <- startPingChecker cfg stationsVar
    (\sid result -> writeBChan chan (EvPingUpdate sid result))

  -- Create initial app state (with reference to event channel), seeded
  -- from the persisted snapshot.
  initialState <- TUI.initialAppState (Just chan) httpMgr cfg persisted
  let initialState' = initialState
        { appAudioCommand = Just (Audio.audioCommand audioEngine)
        , appStationsVar  = Just stationsVar
        }

  -- Build Vty
  vty <- mkVty Vty.defaultConfig

  -- Start background audio state monitor
  _ <- forkIO $ audioMonitorLoop audioEngine chan

  -- Start 1 Hz tick for now-playing scroll
  _ <- forkIO $ tickLoop chan

  -- Run the Brick app (brick-2.12 API). customMain returns the final
  -- AppState; we use it for one last persist call as a safety net
  -- in case the user killed the process with a signal we didn't
  -- handle (the SIGINT/SIGTERM handler fires EvShutdown which already
  -- saves; this is just defense in depth).
  finalState <- customMain vty (pure vty) (Just chan) TUI.app initialState'

  -- Best-effort final save. The in-app EvShutdown handler should
  -- already have saved, but this catches any other exit path.
  _ <- savePersistedState (appToPersisted finalState)

  -- Stop the ping checker thread (it sets its cancel flag and cancels
  -- the async, so the thread is joined before we tear down audio).
  stopPingChecker pingHandle

  -- Cleanup
  Audio.closeAudio audioEngine
  pure ()

--------------------------------------------------------------------------------
-- Background monitor
--------------------------------------------------------------------------------

audioMonitorLoop :: Audio.AudioEngine -> BChan AppEvent -> IO ()
audioMonitorLoop engine eventChan = go
  where
    go = do
      threadDelay 500000
      st  <- Audio.readPlayerState engine
      writeBChan eventChan (EvPlayerUpdate st)
      health <- Audio.readStreamHealth engine
      writeBChan eventChan (EvStreamHealth health)
      vol    <- Audio.readVolume engine
      writeBChan eventChan (EvVolumeUpdate vol)
      -- The metadata TVar is updated on the audio thread (the
      -- Streamly stream's step function) when an ICY 'StreamTitle'
      -- block is consumed. We poll here at 2 Hz so the title
      -- appears in the TUI within ~500 ms of arriving. The
      -- writeBChan always fires so the TUI can also detect a
      -- transition from 'Just title' to 'Nothing' (station switch
      -- or stream end).
      meta  <- Audio.readIcyMetadata engine
      writeBChan eventChan (EvIcyMetaUpdate meta)
      go

-- | 1 Hz 'EvTick' producer. The tick drives the long-title
-- horizontal scroll in the now-playing pane: 'EvTick' handler in
-- 'Tourne.TUI.Events' increments 'appNowPlayingScroll' by 1, and
-- the render function in 'Tourne.TUI.Draw' advances the visible
-- window by the same amount. 1 char/sec is the comfortable
-- "subway-sign" pace — fast enough to be useful, slow enough not
-- to feel frantic. The 'EvTick' constructor already existed (as a
-- no-op handler); this is the first real user.
tickLoop :: BChan AppEvent -> IO ()
tickLoop eventChan = go
  where
    go = do
      threadDelay 1000000
      writeBChan eventChan EvTick
      go
