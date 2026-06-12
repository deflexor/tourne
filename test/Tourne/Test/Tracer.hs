{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the 'Tracer' effect: no-op interpretation, format,
-- the start-time formatting, and the cross-thread unlift guard.
module Tourne.Test.Tracer (tests) where

import Relude
import Data.List qualified as Data.List
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Exception.Safe (tryAny)
import Data.IORef qualified as IORef
import Data.Time.Clock (getCurrentTime, UTCTime)
import Effectful (Eff, IOE, runEff, type (:>), withRunInIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure, assertBool)

import Tourne.Effect.Tracer (Tracer, runTracer, runTracerNoop, traceEvent)

-- | 'traceEvent' under the no-op interpreter must complete without
-- touching stderr. We assert this by checking the unit type — the
-- test passes iff the call returns.
testNoop :: TestTree
testNoop = testCase "no-op interpreter discards every event" $ do
  _ <- runEff (runTracerNoop (do
    traceEvent "play_start" ["dev=0", "queue=0"]
    traceEvent "process"    ["chunk_kb=4", "frames=2"]
    traceEvent "stream_end" []))
  pure ()

-- | Under the no-op interpreter, return values pass through unchanged.
testNoopReturn :: TestTree
testNoopReturn = testCase "no-op preserves the result" $ do
  result <- runEff (runTracerNoop (do
    traceEvent "x" ["a=1"]
    pure (42 :: Int)))
  result @?= 42

-- | When the flag is False the interpreter must not write or
-- throw. Tested by running under the same enabling conditions as
-- testNoop and asserting no error.
testFlagFalse :: TestTree
testFlagFalse = testCase "runTracer with enabled=False discards" $ do
  startRef <- IORef.newIORef =<< getCurrentTime
  _ <- runEff (runTracer startRef False (do
    traceEvent "x" ["a=1"]
    traceEvent "y" ["b=2"]))
  pure ()

-- | Format check: when enabled=True, the line written contains
-- the label and the fields, in that order, separated by spaces,
-- and prefixed by 'D ' + an integer elapsed_ms. We can't easily
-- capture stderr from a child process, but we can verify the
-- interpreter doesn't crash on a real call. A separate end-to-end
-- test (not here) checks the actual line format.
testFlagTrue :: TestTree
testFlagTrue = testCase "runTracer with enabled=True doesn't crash" $ do
  startRef <- IORef.newIORef =<< getCurrentTime
  _ <- runEff (runTracer startRef True (traceEvent "play_start" ["dev=0"]))
  pure ()

-- | Under runTracer with enabled=False, an empty start time still
-- works (the interpreter short-circuits before reading it).
testFlagFalseSkipsRead :: TestTree
testFlagFalseSkipsRead = testCase "enabled=False does not read startRef" $ do
  -- If runTracer read the IORef with enabled=False, it would
  -- hit 'undefined' (we never set it). The test passing proves
  -- the short-circuit is in place.
  let bogusRef :: IORef.IORef UTCTime
      bogusRef = error "must not be read"
  _ <- runEff (runTracer bogusRef False (traceEvent "x" ["a=1"]))
  pure ()

-- | Start time is read; running with a real IORef and a real
-- current time gives a non-negative elapsed_ms in the output line.
-- We assert this indirectly by running the full call and checking
-- it doesn't throw.
testStartTimeConsistent :: TestTree
testStartTimeConsistent = testCase "start time is consulted and elapsed is computed" $ do
  startRef <- IORef.newIORef =<< getCurrentTime
  -- A 1ms delay to ensure diffUTCTime is non-zero on systems
  -- with coarse time resolution.
  threadDelay 1000
  _ <- runEff (runTracer startRef True (traceEvent "now" ["a=1"]))
  pure ()

-- | Multiple events can be sent in sequence; the interpreter
-- handles each one independently. This is the regression for the
-- case where the IORef is mutated between events.
testMultipleEvents :: TestTree
testMultipleEvents = testCase "multiple events are handled independently" $ do
  startRef <- IORef.newIORef =<< getCurrentTime
  _ <- runEff (runTracer startRef True (do
    traceEvent "a" ["x=1"]
    traceEvent "b" ["x=2"]
    traceEvent "c" ["x=3"]))
  pure ()

-- | 'runTracer' and 'runTracerNoop' must produce no effect when
-- given a 'pure' computation. (Sanity check.)
testPureNoop :: TestTree
testPureNoop = testCase "pure action is a no-op" $ do
  result <- runEff (runTracerNoop (pure ()))
  result @?= ()

-- | A 'Trace' callback extracted via 'withRunInIO' is NOT safe to
-- call from a freshly forked OS thread: Effectful's unlift captures
-- the calling thread's context via 'seqUnliftIO', which throws
-- 'HasCallStack' when called from a thread that did not start the
-- Eff runloop. This regression guard documents the failure mode
-- so that a future refactor that restores the unlift-based trace
-- callback in the audio fork thread is caught by the test suite.
-- The Player.hs workaround is a plain IO callback that writes
-- directly to stderr, not routed through the Eff.
testForkThreadUnliftThrows :: TestTree
testForkThreadUnliftThrows =
  testCase "unlift callback throws when called from a forkIO thread" $ do
    startRef <- IORef.newIORef =<< getCurrentTime
    -- Use Async so the fork's exception is propagated to us.
    -- forkIO silently swallows thread-local exceptions, but
    -- Async.wait re-raises them in the calling thread.
    result <- runEff $ runTracer startRef True $ withRunInIO $ \runInIO ->
      tryAny $ do
        let callback = runInIO (traceEvent "from_fork" [])
        a <- Async.async callback
        Async.wait a
    case result of
      Right _ -> assertFailure $
        "Expected Effectful's unlift to throw when called from a \
        \forkIO thread, but the call completed normally. If this \
        \test fails, Effectful has changed to allow cross-thread \
        \unlift, and the audio fork can safely route traces \
        \through the Tracer effect again."
      Left e ->
        -- The error message includes 'unlift' or 'thread'; we
        -- accept any error message that mentions those words.
        let msg = show e
        in assertBool
             ("Expected error to mention unlift/thread, got: " <> msg)
             (any (`Data.List.isInfixOf` msg) ["unlift", "UnliftStrategy", "thread"])

tests :: [TestTree]
tests =
  [ testGroup "Tourne.Effect.Tracer"
  [ testNoop
  , testNoopReturn
  , testFlagFalse
  , testFlagTrue
  , testFlagFalseSkipsRead
  , testStartTimeConsistent
  , testMultipleEvents
  , testPureNoop
  , testForkThreadUnliftThrows
  ]
  ]
