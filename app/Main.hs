module Main (main) where

import Relude
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM qualified as STM
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Main (customMain)
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty qualified as Vty
import System.Posix.Signals (sigINT, sigTERM, installHandler, Handler(Catch))

import Tourne.Types
import Tourne.Config
import Tourne.RadioBrowser qualified as RB
import Tourne.AdDetection qualified as AD
import Tourne.Audio.Player qualified as Audio
import Tourne.PingChecker qualified as Ping
import Tourne.TUI.App qualified as TUI

--------------------------------------------------------------------------------
-- Main entry point
--------------------------------------------------------------------------------

main :: IO ()
main = do
  let cfg = defaultConfig
  chan <- newBChan 100

  -- Initialize audio engine
  audioEngine <- Audio.initAudio

  -- Initialize ad detector
  _adDetector <- AD.initAdDetector

  -- Fetch initial tags in background
  _ <- forkIO $ do
    result <- RB.fetchTags
    case result of
      Right tags -> writeBChan chan (EvTagsLoaded tags)
      Left err   -> writeBChan chan (EvError (toText err))

  -- Install signal handlers for graceful shutdown
  _ <- installHandler sigINT  (Catch (writeBChan chan EvShutdown)) Nothing
  _ <- installHandler sigTERM (Catch (writeBChan chan EvShutdown)) Nothing

  -- Create stations TVar for PingChecker
  stationsVar <- STM.newTVarIO []

  -- Start background ping checker
  _pingHandle <- Ping.startPingChecker cfg stationsVar
    (\sid result -> writeBChan chan (EvPingUpdate sid result))

  -- Create initial app state (with reference to event channel)
  initialState <- TUI.initialAppState cfg (Just chan)
  let initialState' = initialState
        { appAudioCommand = Just (Audio.audioCommand audioEngine)
        , appStationsVar  = Just stationsVar
        }

  -- Build Vty
  vty <- mkVty Vty.defaultConfig

  -- Start background audio state monitor
  _ <- forkIO $ audioMonitorLoop audioEngine chan

  -- Run the Brick app (brick-2.12 API)
  _finalState <- customMain vty (pure vty) (Just chan) TUI.app initialState'

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
      go
