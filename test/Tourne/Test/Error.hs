-- | Tests for 'Tourne.Error.AppError' rendering, kind labels, and JSON
-- round-trip.
module Tourne.Test.Error (tests) where

import Relude
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)

import Tourne.Error

sample :: AppError
sample = HttpError "connection refused"

tests :: [TestTree]
tests =
  [ testGroup "AppError"
  [ testGroup "errorKind"
    [ testCase "HttpError"        $ errorKind (HttpError        "x") @?= "http"
    , testCase "JsonParseError"   $ errorKind (JsonParseError   "x") @?= "json"
    , testCase "DecoderError"     $ errorKind (DecoderError     "x") @?= "decoder"
    , testCase "StreamError"      $ errorKind (StreamError      "x") @?= "stream"
    , testCase "AudioDeviceError" $ errorKind (AudioDeviceError "x") @?= "audio"
    ]
  , testGroup "renderError"
    [ testCase "single-line, prefixed" $
        renderError sample @?= "http: connection refused"

    , testCase "kind is stable, message varies" $ do
        renderError (HttpError "first")  @?= "http: first"
        renderError (HttpError "second") @?= "http: second"

    , testCase "empty message still has kind prefix" $
        renderError (DecoderError "") @?= "decoder: "
    ]
  , testGroup "JSON round-trip"
    [ testCase "HttpError round-trips" $ do
        let bs = Aeson.encode sample
        case Aeson.eitherDecode bs :: Either String AppError of
          Right decoded -> decoded @?= sample
          Left err      -> assertFailure ("decode failed: " <> err)

    , testCase "all variants round-trip" $ do
        let variants =
              [ HttpError        "h"
              , JsonParseError   "j"
              , DecoderError     "d"
              , StreamError      "s"
              , AudioDeviceError "a"
              ]
        forM_ variants $ \v -> do
          let bs = Aeson.encode v
          case Aeson.eitherDecode bs :: Either String AppError of
            Right decoded -> decoded @?= v
            Left err      -> assertFailure
              ("decode failed for " <> show v <> ": " <> err)
    , testCase "unknown kind fails to parse" $ do
        let bad = "{\"kind\":\"bogus\",\"message\":\"x\"}" :: BSL.ByteString
        case Aeson.eitherDecode bad :: Either String AppError of
          Left _  -> pure ()
          Right e -> assertFailure ("expected failure, got: " <> show e)
    ]
  ]
  ]
