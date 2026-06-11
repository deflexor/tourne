-- | Tests for 'Tourne.Types.sortStations' across all three sort modes.
module Tourne.Test.Sorting (tests) where

import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Tourne.Types

-- | Build a Station with optional bitrate and ping, both defaulted to Nothing.
mkStation :: Text -> Maybe Int -> Maybe Double -> Station
mkStation name mBitrate mPing = Station
  { stationId         = StationId name
  , stationName       = name
  , stationUrl        = "http://example/" <> name
  , stationTags       = []
  , stationBitrate    = mBitrate
  , stationCodec      = Nothing
  , stationCountry    = Nothing
  , stationLanguage   = Nothing
  , stationPing       = mPing
  , stationClickCount = Nothing
  }

-- | Convenience: station with no bitrate and no ping.
justName :: Text -> Station
justName n = mkStation n Nothing Nothing

-- | Convenience: station with a known bitrate but no ping.
withBR :: Text -> Int -> Station
withBR n b = mkStation n (Just b) Nothing

-- | Convenience: station with a known ping but no bitrate.
withPing :: Text -> Double -> Station
withPing n p = mkStation n Nothing (Just p)

names :: [Station] -> [Text]
names = fmap stationName

tests :: [TestTree]
tests =
  [ testGroup "sortStations"
  [ testGroup "SortByName"
    [ testCase "empty" $
        names (sortStations SortByName [])
          @?= ([] :: [Text])

    , testCase "already sorted" $
        names (sortStations SortByName
                (fmap justName ["alpha", "bravo", "charlie"]))
          @?= ["alpha", "bravo", "charlie"]

    , testCase "reverse order" $
        names (sortStations SortByName
                (fmap justName ["charlie", "alpha", "bravo"]))
          @?= ["alpha", "bravo", "charlie"]

    , testCase "single element" $
        names (sortStations SortByName [justName "only"])
          @?= ["only"]
    ]

  , testGroup "SortByBitrate"
    [ testCase "highest first" $
        names (sortStations SortByBitrate
                [withBR "lo" 64, withBR "hi" 320, withBR "mid" 128])
          @?= ["hi", "mid", "lo"]

    , testCase "ties broken by name" $
        names (sortStations SortByBitrate
                [withBR "b" 128, withBR "a" 128, withBR "c" 128])
          @?= ["a", "b", "c"]

    , testCase "missing bitrate sorts to the end" $
        names (sortStations SortByBitrate
                [ withBR "known"     128
                , justName "unknown"
                , withBR "also-known" 256
                ])
          @?= ["also-known", "known", "unknown"]

    , testCase "all missing bitrates fall back to name order" $
        names (sortStations SortByBitrate
                (fmap justName ["charlie", "alpha", "bravo"]))
          @?= ["alpha", "bravo", "charlie"]
    ]

  , testGroup "SortByPing"
    [ testCase "lower ping first" $
        names (sortStations SortByPing
                [ withPing "slow" 0.5
                , withPing "fast" 0.05
                , withPing "mid"  0.2
                ])
          @?= ["fast", "mid", "slow"]

    , testCase "unknown ping sorts last" $
        names (sortStations SortByPing
                [ withPing "known"   0.1
                , justName  "unknown"
                , withPing "slow"    0.9
                ])
          @?= ["known", "slow", "unknown"]

    , testCase "both unknown: fall back to name" $
        names (sortStations SortByPing
                [justName "charlie", justName "alpha"])
          @?= ["alpha", "charlie"]
    ]
  ]
  ]
