{-# LANGUAGE NumericUnderscores #-}

-- | Tests for the pull-based decode loop iteration pattern used by
-- 'Tourne.Audio.Player.decodeLoopStream'.
module Tourne.Test.Stream (tests) where

import Relude
import Data.ByteString.Char8 qualified as BSC
import Streamly.Data.Stream.Prelude (Stream, unfoldrM)
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | Build a 'Stream IO ByteString' from a list of chunks.
listToStream :: [ByteString] -> Stream IO ByteString
listToStream chunks =
  unfoldrM step chunks
  where
    step [] = pure Nothing
    step (x:xs) = pure (Just (x, xs))

tests :: [TestTree]
tests =
  [ testGroup "Tourne.Audio.Stream"
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
