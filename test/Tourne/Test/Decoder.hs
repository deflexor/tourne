{-# LANGUAGE BangPatterns #-}

-- | Tests for the actual mpg123 decoder.
--
-- The original tourne audio bug was: ~32 KB of MP3 streamed from
-- radio.plaza.one decoded to zero PCM frames, leaving the UI
-- stuck on "Buffering 0/2048 KB" forever. Root cause:
-- 'Decoder.mpg123Open' never called 'mpg123_format', so mpg123
-- never agreed on an output format and 'mpg123_read' either
-- refused to read or returned 0 bytes. The fix in 'mpg123Open'
-- forces 44100 Hz / 2 channels / signed 16-bit, matching the
-- SDL2 device opened in 'Tourne.Audio.Player.initAudio'.
--
-- This test fetches a small chunk of a real radio stream and
-- asserts that the decoder produces at least one PCM frame.
-- Before the fix it returned 'Right []'; after the fix it
-- returns 'Right frames' with non-empty 'afPcmData'.
module Tourne.Test.Decoder (tests) where

import Relude
import Data.ByteString qualified as BS
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Exception.Safe (tryAny)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)

import Tourne.Audio.Decoder (mpg123Open, mpg123Close, mpg123Feed)
import Tourne.Audio.Types (afPcmData)

-- | Fetch up to 64 KB of a live radio stream. The 'Maybe' return
-- lets callers skip the test gracefully if the network is
-- unavailable; the regression guard is the assertion on frames.
fetchLiveBytes :: IO (Maybe ByteString)
fetchLiveBytes = do
  result <- tryAny $ do
    streamReq <- HC.parseRequest "http://radio.plaza.one/mp3"
    mgr <- HC.newManager tlsManagerSettings
    HC.withResponse streamReq mgr $ \response -> do
      let body = HC.responseBody response
      readLimited body 65536 BS.empty
  pure $ case result of
    Right bs -> Just bs
    Left _   -> Nothing
  where
    readLimited body remaining !acc
      | remaining <= 0 = pure acc
      | otherwise = do
          chunk <- HC.brRead body
          if BS.null chunk
            then pure acc
            else
              let used = min remaining (BS.length chunk)
                  kept = BS.take used chunk
                  rest = BS.drop used chunk
              in readLimited body (remaining - used) (acc <> kept <> rest)

tests :: [TestTree]
tests =
  [ testGroup "Tourne.Audio.Decoder"
  [ testCase "feeding live MP3 produces at least one PCM frame" $ do
      mBytes <- fetchLiveBytes
      case mBytes of
        Nothing ->
          assertFailure
            "Network unavailable: cannot fetch http://radio.plaza.one/mp3"
        Just bs -> do
          mpResult <- mpg123Open
          case mpResult of
            Left err ->
              assertFailure $ "mpg123Open failed: " <> show err
            Right h -> do
              feedResult <- mpg123Feed h bs
              case feedResult of
                Left err ->
                  assertFailure $ "mpg123Feed failed: " <> show err
                Right [] ->
                  assertFailure $
                    "Decoded 0 frames from "
                      <> show (BS.length bs)
                      <> " bytes of live MP3. This is the \
                        \'Buffering 0/2048 KB' regression — \
                        \mpg123Open must call mpg123_format to \
                        \agree on an output rate/channels/encoding."
                Right frames -> do
                  let totalBytes = sum (map (BS.length . afPcmData) frames)
                  when (totalBytes <= 0) $
                    assertFailure $
                      "Frames list is non-empty but contains \
                      \0 PCM bytes total."
              mpg123Close h
  ]
  ]
