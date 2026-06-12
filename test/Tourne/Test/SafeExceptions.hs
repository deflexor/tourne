-- | Tests for the exception-discipline change in Tourne: the codebase
-- now uses 'tryAny' from 'Control.Exception.Safe' instead of
-- @try @SomeException@ from 'Control.Exception'. The key property we
-- test is that async exceptions are NOT swallowed.
module Tourne.Test.SafeExceptions (tests) where

import Relude
import Control.Concurrent.Async (async, wait, cancelWith)
import Control.Exception
  ( throwIO, AsyncException (..), ArithException, fromException, evaluate
  , try, SomeException
  )
import Control.Exception.Safe (tryAny)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool)

tests :: [TestTree]
tests =
  [ testGroup "Exception discipline"
  [ testCase "tryAny re-throws ThreadKilled (not caught)" $ do
      -- tryAny uses Control.Exception.Safe.try, which re-throws
      -- asynchronous exceptions instead of catching them. So calling
      -- tryAny (throwIO ThreadKilled) must propagate ThreadKilled
      -- out of the call. We catch it with a broader @try @SomeException@
      -- to verify the right exception was re-thrown.
      outer <- (try @SomeException (tryAny (throwIO ThreadKilled :: IO ())))
      case outer of
        Right (Right _) -> assertFailure "tryAny returned Right _"
        Right (Left _)   -> assertFailure "tryAny returned Left _ (caught async!)"
        Left e          -> case fromException e of
          Just ThreadKilled -> pure ()
          _                 -> assertFailure
            ("expected ThreadKilled, got: " <> show e)

  , testCase "tryAny catches synchronous exceptions" $ do
      -- Sanity check: the synchronous case (a regular ArithException)
      -- is still caught. This is the normal use-case.
      result <- tryAny (evaluate (1 `div` (0 :: Int)))
      case result of
        Right _  -> assertFailure "tryAny did not catch divide by zero"
        Left e   -> assertBool "exception captured"
                       (isJust (fromException e :: Maybe ArithException))

  , testCase "cancelWith propagates past inner tryAny" $ do
      -- The realistic scenario: an HTTP request is wrapped in
      -- tryAny; the user cancels the parent async. The cancel
      -- must NOT be turned into an AppError. Since tryAny
      -- re-throws async, wait must throw ThreadKilled to us.
      a <- async (tryAny (throwIO ThreadKilled :: IO ()))
      cancelWith a ThreadKilled
      result <- (try @SomeException (wait a))
      case result of
        Right _  -> assertFailure "tryAny swallowed or caught ThreadKilled"
        Left e   -> case fromException e of
          Just ThreadKilled -> pure ()
          _                 -> assertFailure
            ("expected ThreadKilled, got: " <> show e)
  ]
  ]
