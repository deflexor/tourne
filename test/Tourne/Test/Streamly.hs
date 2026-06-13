{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the Streamly-based stream wiring that we use as a
-- reference for the audio streaming refactor. These tests pin
-- down the Streamly idioms we depend on, so that a future
-- streamly upgrade that breaks the API is caught here rather
-- than in the runtime audio code.
module Tourne.Test.Streamly (tests) where

import Relude
import Data.ByteString qualified as Data.ByteString
import Streamly.Data.Stream.Prelude (Stream, fromPure, unfoldrM, before)
import qualified Streamly.Data.Stream.Prelude as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

-- | 'unfoldrM' should produce a stream from a stateful
-- generator. With a finite generator (N -> Nothing), toList
-- yields exactly N elements. This is the same shape we'll use
-- in Tourne.Audio.Stream: the BodyReader is the state, each
-- step is one brRead, the stream ends when brRead returns
-- empty.
testUnfoldrFinite :: TestTree
testUnfoldrFinite = testCase "unfoldrM over a finite state yields N elements" $ do
  let n = 5
      step :: Int -> IO (Maybe (Int, Int))
      step i
        | i < n = pure (Just (i, i + 1))
        | otherwise = pure Nothing
  out <- S.toList $ unfoldrM step 0
  out @?= [0, 1, 2, 3, 4]

-- | 'before' runs the given action each time the stream is
-- iterated. This is the property we need to be aware of for
-- HTTP connection opening: if we naively put 'HC.withResponse'
-- in 'before', each new consumer will open a fresh response.
-- We need a Streamly primitive that runs once at construction
-- instead (e.g. 'Streamly.Data.Stream.Prelude.fromEffect' for
-- an initial pure state, or a manual bracket via 'before' +
-- a finalizer). Documenting the actual behaviour here so the
-- audio refactor doesn't make this mistake.
testBeforeRunsEachIteration :: TestTree
testBeforeRunsEachIteration = testCase "before runs the side effect on each iteration" $ do
  ref <- newIORef (0 :: Int)
  let increment = modifyIORef' ref (+ 1)
      stream = before increment (fromPure 42 :: Stream IO Int)
  _ <- S.toList stream
  n1 <- readIORef ref
  _ <- S.toList stream
  n2 <- readIORef ref
  n1 @?= 1
  n2 @?= 2
  -- 'before' is per-iteration, not once-per-stream. For HTTP
  -- connection opening, we'll use a different approach
  -- (a finalizer pattern with 'Streamly.Data.Stream.before' +
  -- a separate cleanup, or a state-passing technique).

-- | A stream that produces no elements (the body reader
-- returns empty immediately) should yield the empty list when
-- forced.
testEmptyStream :: TestTree
testEmptyStream = testCase "unfoldrM that returns Nothing on the first step yields []" $ do
  let step :: Int -> IO (Maybe (Int, Int))
      step _ = pure Nothing
  out <- S.toList $ unfoldrM step 0
  out @?= ([] :: [Int])

-- | A stream of MP3 chunks (the production shape) should be
-- produceable from a list of fixed bytestrings. This is what
-- we'll do for tests: instead of going through HTTP, we
-- pre-record the bytes and feed them through a streamly
-- unfoldrM so the rest of the pipeline can be tested
-- without a real network.
testByteStringStream :: TestTree
testByteStringStream = testCase "unfoldrM over [ByteString] yields the list in order" $ do
  let input :: [ByteString]
      input = ["a", "b", "c"]
      step [] = pure Nothing
      step (x:xs) = pure (Just (x, xs))
  out <- S.toList $ unfoldrM step input
  out @?= input
  -- Sanity: total bytes match.
  assertBool "total bytes" (sum (map Data.ByteString.length out) == 3)

-- | The stream is pull-based: the step function is not invoked
-- until the consumer asks for an element. We use a counter
-- IORef to verify that toList only triggers as many steps as
-- needed.
testPullSemantics :: TestTree
testPullSemantics = testCase "steps are not run until consumed (pull-based)" $ do
  ref <- newIORef (0 :: Int)
  let step i = do
        modifyIORef' ref (+ 1)
        pure (Just (i * 2, i + 1))
  -- Take only the first 3 elements.
  let s = unfoldrM step (0 :: Int)
  out <- S.toList $ S.take 3 s
  n <- readIORef ref
  out @?= [0, 2, 4]
  -- Step ran exactly 3 times.
  n @?= 3

tests :: [TestTree]
tests =
  [ testGroup "Tourne.Test.Streamly"
  [ testUnfoldrFinite
  , testBeforeRunsEachIteration
  , testEmptyStream
  , testByteStringStream
  , testPullSemantics
  ]
  ]
