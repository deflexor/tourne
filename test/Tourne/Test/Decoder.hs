{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

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
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM qualified as STM
import Control.Exception.Safe (tryAny)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool)

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

-- | Open a streaming connection, count bytes received over the
-- given number of seconds, and return the total. Returns 'Nothing'
-- on any network error so the caller can skip the test offline.
streamFiveSeconds :: Int -> IO (Maybe Int)
streamFiveSeconds seconds = do
  result <- tryAny $ do
    streamReq <- HC.parseRequest "http://radio.plaza.one/mp3"
    mgr <- HC.newManager tlsManagerSettings
    let req = streamReq
          { HC.responseTimeout = HC.responseTimeoutNone
          , HC.checkResponse = \_ _ -> pure ()  -- accept any status
          }
    HC.withResponse req mgr $ \response -> do
      let body = HC.responseBody response
      let deadlineMicros = seconds * 1_000_000
      countFor deadlineMicros body 0
  pure $ case result of
    Right n -> Just n
    Left _  -> Nothing
  where
    countFor budget body !acc
      | budget <= 0 = pure acc
      | otherwise = do
          chunk <- HC.brRead body
          let acc' = acc + BS.length chunk
          if BS.null chunk
            then pure acc'
            else do
              -- burn 50 ms of the budget per read
              threadDelay 50_000
              countFor (budget - 50_000) body acc'

-- | Mirror the production 'feedStream' pipeline: forkIO reads
-- from the HTTP body in 32 KB batches and writes to a TChan. A
-- consumer polls the channel for 'seconds' seconds and counts the
-- bytes received. If the fork dies early, the consumer reports a
-- low count.
simulateFeedStreamPipeline :: Int -> IO (Maybe Int)
simulateFeedStreamPipeline seconds = do
  result <- tryAny $ do
    streamReq <- HC.parseRequest "http://radio.plaza.one/mp3"
    mgr <- HC.newManager tlsManagerSettings
    let req = streamReq
          { HC.responseTimeout = HC.responseTimeoutNone
          , HC.checkResponse = \_ _ -> pure ()
          }
    chan <- STM.newTChanIO
    HC.withResponse req mgr $ \response -> do
      let body = HC.responseBody response
          producer = do
            let go !acc = do
                  chunk <- HC.brRead body
                  if BS.null chunk
                    then STM.atomically $ STM.writeTChan chan BS.empty
                    else do
                      let acc' = acc <> chunk
                      -- Match production's 32 KB minBatchSize.
                      if BS.length acc' >= 32768
                        then do
                          STM.atomically $ STM.writeTChan chan acc'
                          go BS.empty
                        else go acc'
            go BS.empty
      _ <- forkIO producer
      let consumer = consumeFor (seconds * 1_000_000) chan 0
      consumer
  pure $ case result of
    Right n -> Just n
    Left _  -> Nothing
  where
    consumeFor budget !chan !acc
      | budget <= 0 = pure acc
      | otherwise = do
          mb <- STM.atomically $ STM.tryReadTChan chan
          threadDelay 50_000
          case mb of
            Nothing -> consumeFor (budget - 50_000) chan acc
            Just bs
              | BS.null bs -> pure acc
              | otherwise -> consumeFor (budget - 50_000) chan (acc + BS.length bs)

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

  , testCase "streamed MP3 over 5s delivers at least 50 KB" $ do
      -- Regression guard for the "1 second of audio then silence"
      -- bug. The streaming pipeline should keep delivering data
      -- from a live radio stream for the duration of this test.
      -- If the body reader goes silent after the first chunk, the
      -- audio decoder stalls and the user hears ~1 s of music
      -- followed by silence. The threshold is intentionally low
      -- (~50 KB / 5s) to avoid CI flakes: the live stream
      -- typically delivers 100-300 KB in 5 s, but a slow
      -- connection could deliver less. The point is to fail if
      -- the body reader truly stops.
      result <- streamFiveSeconds 5
      case result of
        Nothing ->
          assertFailure
            "Network unavailable: cannot fetch \
            \http://radio.plaza.one/mp3"
        Just totalBytes ->
          assertBool
            ("Expected at least 50 KB from a 5s live stream, got "
              <> show totalBytes <> " bytes")
            (totalBytes >= 50_000)

  , testCase "forkIO feedStream pipeline keeps accumulating over 5s" $ do
      -- The actual production path: a forked thread reads from the
      -- HTTP body, batches into 8 KB chunks, and writes to a
      -- TChan. A consumer polls the channel for 5 s and counts
      -- the bytes received. If the forked thread silently dies
      -- (e.g. uncaught exception in HC.brRead, or http-client
      -- closes the body early), this test fails.
      result <- simulateFeedStreamPipeline 5
      case result of
        Nothing ->
          assertFailure
            "Network unavailable: cannot fetch \
            \http://radio.plaza.one/mp3"
        Just totalBytes ->
          assertBool
            ("Expected at least 50 KB from a 5s feedStream \
            \pipeline, got " <> show totalBytes <> " bytes")
            (totalBytes >= 50_000)
  ]
  ]
