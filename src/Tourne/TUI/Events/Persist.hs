{-|
Module      : Tourne.TUI.Events.Persist
Description : Persistence-dispatch helper.

'schedulePersist' triggers a background save by pushing
'EvPersistNow' onto the Brick event channel. The save itself
happens in the @Main@ thread (it reads 'AppState' and writes
the state.json file), not inline in the TUI handler.

The fallback path (when the channel isn't wired up, e.g. in
misconfigured tests) writes the snapshot directly via
'savePersistedState'.
-}
module Tourne.TUI.Events.Persist (schedulePersist) where

import Relude
import Brick.BChan (writeBChan)
import Brick.Types (EventM, gets)

import Tourne.Types
import Tourne.Persistence (appToPersisted, savePersistedState)
import Tourne.TUI.Core (AppName)

-- | Trigger an 'EvPersistNow' via the event channel so a save
-- happens shortly. Falls back to a direct save if the channel
-- isn't wired up (which would only happen in misconfigured test
-- setups).
schedulePersist :: EventM AppName AppState ()
schedulePersist = do
  mChan <- gets appEventChan
  case mChan of
    Just chan -> liftIO $ void $ writeBChan chan EvPersistNow
    Nothing   -> do
      st <- get
      liftIO $ void $ savePersistedState (appToPersisted st)
