{-# LANGUAGE NoImplicitPrelude #-}

-- | Process-wide shared HTTP 'Manager'.
--
-- A 'HC.Manager' owns a connection pool and a TLS context. Creating one per
-- request is an anti-pattern: it defeats keep-alive, forces TLS
-- re-negotiation on every call, and leaks pooled connections. Create once,
-- share everywhere.
--
-- This module is a temporary bridge: in Phase B (effectful adoption) the
-- shared 'HC.Manager' becomes a proper @effectful@ effect.
module Tourne.Http
  ( getSharedManager
  ) where

import Relude hiding (MVar, newMVar)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO.Unsafe (unsafePerformIO)

-- | Top-level memoized Manager cell.
--
-- The single 'unsafePerformIO' only seeds an empty 'MVar' (idempotent); the
-- 'HC.Manager' itself is built lazily on first call under the 'MVar' lock, so
-- it is created exactly once even under contention. This is the
-- http-client-recommended sharing pattern for apps without a DI framework.
managerCell :: MVar (Maybe HC.Manager)
managerCell = unsafePerformIO (newMVar Nothing)
{-# NOINLINE managerCell #-}

-- | Obtain the shared HTTP 'HC.Manager', creating it on first call
-- (thread-safe).
getSharedManager :: IO HC.Manager
getSharedManager = modifyMVar managerCell \case
  Just mgr -> pure (Just mgr, mgr)
  Nothing  -> do
    mgr <- HC.newManager tlsManagerSettings
    pure (Just mgr, mgr)
