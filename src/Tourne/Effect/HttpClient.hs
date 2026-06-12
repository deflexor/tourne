{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Tourne.Effect.HttpClient
Description : Effectful access to the shared HTTP 'Manager'.

A single operation: 'getManager' returns the process-wide
@http-client@ 'Manager'. The 'Manager' is built once (lazily on
first use) and shared across all HTTP calls, so connection pooling
and TLS state are reused.

The whole module replaces the previous
@unsafePerformIO + MVar@ trick in 'Tourne.Http' with a proper
effectful effect. The interpreter is set up in 'Main' (or wherever
the @Manager@ should be allocated) and threads the same 'Manager'
to every caller. Tests can use 'runHttpClientNoop' (or a custom
interpreter) to substitute a mock 'Manager' for testing.

The 'Manager' is intentionally lazy: tests that never call
'getManager' never need to construct one.
-}
module Tourne.Effect.HttpClient
  ( -- * Effect
    HttpClient
  , getManager
    -- * Interpreters
  , runHttpClient
  , runHttpClientNoop
  ) where

import Relude
import Effectful (Dispatch (Dynamic), DispatchOf, Effect, Eff, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Network.HTTP.Client (Manager)
import Effectful.Internal.Monad (send)

-- | The HTTP-client effect. Carries no payload of its own; the
-- 'Manager' is held by the interpreter and accessed via
-- 'getManager'.
data HttpClient :: Effect where
  GetManager :: HttpClient m Manager

type instance DispatchOf HttpClient = 'Dynamic

-- | Get the shared HTTP 'Manager'. The 'Manager' is built once
-- (lazily on first call) and reused for every subsequent
-- 'getManager' call.
getManager :: (HttpClient :> es) => Eff es Manager
getManager = send GetManager

--------------------------------------------------------------------------------
-- Interpreters
--------------------------------------------------------------------------------

-- | Run with a pre-built 'Manager'. Pass the result of
-- 'Network.HTTP.Client.newManager' (or a 'Manager' you've built
-- once at startup). The 'Manager' is held in the closure; every
-- 'getManager' returns the same value.
runHttpClient
  :: (IOE :> es)
  => Manager
  -> Eff (HttpClient : es) a
  -> Eff es a
runHttpClient mgr = interpret_ $ \GetManager -> pure mgr

-- | Discard every 'GetManager' call with a placeholder. Useful
-- for tests that never actually make an HTTP request.
--
-- WARNING: the placeholder is *not* a real 'Manager'; if your
-- code path tries to use it for a real request, you'll get a
-- runtime error. Use 'runHttpClient' in production.
runHttpClientNoop :: Eff (HttpClient : es) a -> Eff es a
runHttpClientNoop = interpret_ $ \GetManager -> error "runHttpClientNoop: getManager was called"
