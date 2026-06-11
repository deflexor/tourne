-- | Tests for 'Tourne.Persistence':
--   * round-trip encode/decode
--   * v1 schema (no 'psStationSort') is accepted as v2 via the custom FromJSON
module Tourne.Test.Persistence (tests) where

import Relude
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict qualified as HashMap
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)

import Tourne.Persistence
import Tourne.Types

-- | A minimal non-default PersistedState used in several tests.
sampleState :: PersistedState
sampleState = defaultPersistedState
  { psTags            = [Tag "jazz" 42, Tag "rock" 17]
  , psCurrentTag      = Just "jazz"
  , psSelectedStation = Just (StationId "abc-123")
  , psVolume          = 0.42
  , psFocus           = FocusStations
  , psStationSort     = SortByBitrate
  , psStationsByTag   = HashMap.fromList
      [ ("jazz", [mkStation "WJZZ" (Just 192) Nothing])
      ]
  , psWasPlaying      = True
  }
  where
    mkStation :: Text -> Maybe Int -> Maybe Double -> Station
    mkStation n br ping = Station
      { stationId         = StationId (n <> "-id")
      , stationName       = n
      , stationUrl        = "http://example/" <> n
      , stationTags       = []
      , stationBitrate    = br
      , stationCodec      = Nothing
      , stationCountry    = Nothing
      , stationLanguage   = Nothing
      , stationPing       = ping
      , stationClickCount = Nothing
      }

-- | Encode/decode helper that throws on a round-trip mismatch.
-- Using 'error' here is safe: a failure means our Persistence
-- code is broken, not bad test data.
roundTrip :: PersistedState -> PersistedState
roundTrip s = case decodePersistedState (encodePersistedState s) of
  Right s' -> s'
  Left err -> error ("roundTrip failed: " <> err)

-- | A v1 schema JSON literal (no 'psStationSort' field). Mirrors the shape
-- of the on-disk cache from before the v2 bump.
--   * 'psFocus' uses the Generic-derived constructor name ("FocusStations").
v1JsonLiteral :: BSL.ByteString
v1JsonLiteral = "{\"psVersion\":2,\"psTags\":[{\"name\":\"jazz\",\"stationcount\":42}],\
                  \\"psStationsByTag\":{},\"psCurrentTag\":\"jazz\",\
                  \\"psSelectedStation\":\"abc-123\",\"psVolume\":0.42,\
                  \\"psFocus\":\"FocusStations\",\
                  \\"psTagsCursor\":{\"listSelected\":0,\"listOffset\":0,\"listSize\":0},\
                  \\"psStationsCursor\":{\"listSelected\":0,\"listOffset\":0,\"listSize\":0},\
                  \\"psWasPlaying\":true,\"psResumeStationUrl\":null}"

tests :: [TestTree]
tests =
  [ testGroup "Persistence"
  [ testGroup "encodePersistedState / decodePersistedState"
    [ testCase "default round-trips" $
        roundTrip defaultPersistedState @?= defaultPersistedState

    , testCase "populated state round-trips" $
        roundTrip sampleState @?= sampleState

    , testCase "empty bytestring is a parse error, not a crash" $ do
        case decodePersistedState BSL.empty of
          Left _  -> pure ()
          Right s -> assertFailure
            ("expected Left for empty input, got Right: " <> show s)
    ]
  , testGroup "schema versioning"
    [ testCase "v1 file (no psStationSort) decodes with default SortByName" $
        case decodePersistedState v1JsonLiteral of
          Right s  -> do
            psVersion s @?= 2
            psStationSort s @?= SortByName
            psVolume s @?= 0.42
          Left err -> assertFailure ("v1 decode failed: " <> toString err)
    ]
  ]
  ]
