module Tourne.Config where

import Relude

data Config = Config
  { configBufferSize      :: !Int       -- ^ Audio ring buffer size in PCM frames
  , configFailover        :: !Bool      -- ^ Enable automatic failover
  , configAdDetection     :: !Bool      -- ^ Enable ad detection
  , configMaxPingTime     :: !Double    -- ^ Max ping time in seconds
  , configApiBaseUrl      :: !Text      -- ^ Radio browser API base URL
  , configDefaultTags     :: ![Text]    -- ^ Tags to show on startup
  , configMaxStations     :: !Int       -- ^ Max stations per tag
  , configPlayerVolume    :: !Double    -- ^ Default volume (0-1)
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { configBufferSize      = 4096
  , configFailover        = True
  , configAdDetection     = False
  , configMaxPingTime     = 5.0
  , configApiBaseUrl      = "https://de1.api.radio-browser.info"
  , configDefaultTags     = ["jazz", "classical", "rock", "pop", "electronic", "news"]
  , configMaxStations     = 100
  , configPlayerVolume    = 0.8
  }
