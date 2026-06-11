module Tourne.Config where

import Relude

data Config = Config
  { configBufferSize          :: !Int       -- ^ Audio ring buffer size in PCM frames
  , configFailover            :: !Bool      -- ^ Enable automatic failover
  , configAdDetection         :: !Bool      -- ^ Enable ad detection
  , configMaxPingTime         :: !Double    -- ^ Max ping time in seconds
  , configApiBaseUrl          :: !Text      -- ^ Radio browser API base URL
  , configDefaultTags         :: ![Text]    -- ^ Tags to show on startup
  , configMaxStations         :: !Int       -- ^ Max stations per tag
  , configPlayerVolume        :: !Double    -- ^ Default volume (0-1)
  , configPingBatchSize       :: !Int       -- ^ Stations to ping per cycle
  , configPingIntervalSeconds :: !Int       -- ^ Seconds between ping cycles
  , configPingConnectTimeout  :: !Int       -- ^ HTTP connect timeout (seconds)
  , configPingResponseTimeout :: !Int       -- ^ HTTP response timeout (seconds)
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { configBufferSize          = 4096
  , configFailover            = True
  , configAdDetection         = False
  , configMaxPingTime         = 5.0
  , configApiBaseUrl          = "https://de1.api.radio-browser.info"
  , configDefaultTags         = ["jazz", "classical", "rock", "pop", "electronic", "news"]
  , configMaxStations         = 100
  , configPlayerVolume        = 0.8
  , configPingBatchSize       = 10         -- 10 stations every 30s
  , configPingIntervalSeconds = 30         -- = ~20 req/min, 0.33 req/s
  , configPingConnectTimeout  = 5          -- 5s TCP connect
  , configPingResponseTimeout = 10         -- 10s response header
  }
