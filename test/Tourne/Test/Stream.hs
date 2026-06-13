{-# LANGUAGE NumericUnderscores #-}

-- | Tests for the stream-to-handle shim used during the Streamly
-- refactor transition.
module Tourne.Test.Stream (tests) where

import Relude
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Control.Concurrent (threadDelay)
import Streamly.Data.Stream.Prelude (Stream, unfoldrM)
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Tourne.Audio.Stream (readStreamChunk, closeStream, shCancel)
import Tourne.Audio.Stream.Shim (streamToHandle)

-- | Build a 'Stream IO ByteString' from a list of chunks.
listToStream :: [ByteString] -> Stream IO ByteString
listToStream chunks =
  unfoldrM step chunks
  where
    step [] = pure Nothing
    step (x:xs) = pure (Just (x, xs))

tests :: [TestTree]
tests =
  [ testGroup "Tourne.Audio.Stream.Shim"
  [ testCase "streamToHandle yields all chunks in order" $ do
      let input = ["chunk-a", "chunk-b", "chunk-c"]
      handle <- streamToHandle (listToStream (fmap BSC.pack input))
      c1 <- readStreamChunk handle
      c2 <- readStreamChunk handle
      c3 <- readStreamChunk handle
      c4 <- readStreamChunk handle
      c1 @?= Just "chunk-a"
      c2 @?= Just "chunk-b"
      c3 @?= Just "chunk-c"
      c4 @?= Nothing
      closeStream handle

  , testCase "streamToHandle empty stream yields Nothing" $ do
      handle <- streamToHandle (listToStream [])
      c <- readStreamChunk handle
      c @?= Nothing
      closeStream handle

  , testCase "streamToHandle single chunk" $ do
      handle <- streamToHandle (listToStream ["hello"])
      c <- readStreamChunk handle
      c @?= Just "hello"
      closeStream handle

  , testCase "shCancel signals EOF and stops iteration" $ do
      -- Use an MVar to block the stream's first step so we can
      -- control the timing of cancellation.
      gate <- newEmptyMVar
      let blockingStream = unfoldrM step (0 :: Int)
            where
              step 0 = do
                takeMVar gate
                pure (Just ("alpha", 1))
              step 1 = do
                threadDelay 1_000_000_000  -- 1 s, long enough to never finish
                pure (Just ("beta", 2))
              step _ = pure Nothing
      handle <- streamToHandle blockingStream
      -- Unblock the stream so "alpha" is produced.
      putMVar gate ()
      c1 <- readStreamChunk handle
      c1 @?= Just "alpha"
      -- Cancel before "beta" arrives.
      shCancel handle
      -- The shim writes the EOF marker; the forked thread is killed.
      c2 <- readStreamChunk handle
      c2 @?= Nothing

  , testCase "streamToHandle preserves binary data including nulls" $ do
      let input = [BS.singleton 0, BS.singleton 255, BS.singleton 128]
      handle <- streamToHandle (listToStream input)
      c1 <- readStreamChunk handle
      c2 <- readStreamChunk handle
      c3 <- readStreamChunk handle
      c4 <- readStreamChunk handle
      c1 @?= Just (BS.singleton 0)
      c2 @?= Just (BS.singleton 255)
      c3 @?= Just (BS.singleton 128)
      c4 @?= Nothing
      closeStream handle

  , testCase "streamToHandle with large stream" $ do
      let n = (1000 :: Int)
          input = fmap (\i -> BSC.pack (show i)) [1 .. n]
      handle <- streamToHandle (listToStream input)
      -- Read all chunks back
      let readAll acc = do
            mc <- readStreamChunk handle
            case mc of
              Nothing -> pure (reverse acc)
              Just c  -> readAll (c : acc)
      output <- readAll []
      output @?= input
      closeStream handle
  ]

  , testGroup "Tourne.Audio.Stream.decodeLoop"
  [ testCase "foldBreak one allows sequential pull iteration" $ do
      -- This exercises the same pull pattern used by
      -- decodeLoopStream: each call to foldBreak Fold.one
      -- extracts one chunk and returns the remaining stream.
      let input = ["chunk-a", "chunk-b", "chunk-c"]
          stream = listToStream (fmap BSC.pack input)
      (m1, s1) <- S.foldBreak Fold.one stream
      (m2, s2) <- S.foldBreak Fold.one s1
      (m3, s3) <- S.foldBreak Fold.one s2
      (m4, _)  <- S.foldBreak Fold.one s3
      m1 @?= Just "chunk-a"
      m2 @?= Just "chunk-b"
      m3 @?= Just "chunk-c"
      m4 @?= Nothing

  , testCase "foldBreak one on empty stream returns Nothing immediately" $ do
      (m, _) <- S.foldBreak Fold.one (listToStream [] :: Stream IO ByteString)
      m @?= Nothing

  , testCase "foldBreak one loop consumes all elements" $ do
      let n = (100 :: Int)
          input = fmap (\i -> BSC.pack (show i)) [1 .. n]
      let consume acc s = do
            (mChunk, rest) <- S.foldBreak Fold.one s
            case mChunk of
              Nothing -> pure (reverse acc)
              Just c  -> consume (c : acc) rest
      output <- consume [] (listToStream input)
      output @?= input
  ]
  ]
