module Tourne.AdDetection
  ( AdDetector
  , initAdDetector
  , feedVolumeSample
  , getAdState
  ) where

import Relude
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TVar qualified as STM
import Data.List qualified as List
import Tourne.Types

--------------------------------------------------------------------------------
-- Ad detector state
--------------------------------------------------------------------------------

data AdDetector = AdDetector
  { adSamplesWindow :: !(STM.TVar [Double])
  , adSuspicionVar  :: !(STM.TVar Double)
  , adConfig        :: !AdDetectorConfig
  } deriving (Generic)

data AdDetectorConfig = AdDetectorConfig
  { adcWindowSize     :: !Int
  , adcThresholdRise  :: !Double
  , adcSuspectLevel   :: !Double
  , adcMinVariability :: !Double
  } deriving (Eq, Show)

defaultAdConfig :: AdDetectorConfig
defaultAdConfig = AdDetectorConfig
  { adcWindowSize     = 50
  , adcThresholdRise  = 2.5
  , adcSuspectLevel   = 0.7
  , adcMinVariability = 0.3
  }

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

initAdDetector :: IO AdDetector
initAdDetector = do
  samplesVar   <- STM.newTVarIO []
  suspicionVar <- STM.newTVarIO 0.0
  pure AdDetector
    { adSamplesWindow = samplesVar
    , adSuspicionVar  = suspicionVar
    , adConfig        = defaultAdConfig
    }

--------------------------------------------------------------------------------
-- Volume analysis
--------------------------------------------------------------------------------

feedVolumeSample :: AdDetector -> Double -> IO AdState
feedVolumeSample detector volume = do
  suspicion <- STM.atomically $ do
    samples <- STM.readTVar (adSamplesWindow detector)
    let windowN = adcWindowSize (adConfig detector)
        newSamples = take windowN (volume : samples)
    STM.writeTVar (adSamplesWindow detector) newSamples

    let n      = length newSamples
        avg    = sum newSamples / fromIntegral (max 1 n)
        maxV   = List.maximum newSamples
        minV   = List.minimum newSamples
        range  = maxV - minV
        normVar = if avg > 0.001 then range / avg else 0

    let prevAvg = if length samples > windowN
                  then sum (take windowN samples) / fromIntegral windowN
                  else if null samples then 0
                       else sum samples / fromIntegral (length samples)

        volumeRise = if prevAvg > 0.001 then avg / prevAvg else 1.0
        isSuspicious = normVar < adcMinVariability (adConfig detector)
                    && volumeRise > adcThresholdRise (adConfig detector)
                    && avg > 0.2

    currentSuspicion <- STM.readTVar (adSuspicionVar detector)
    let newSuspicion
          | isSuspicious                     = min 1.0 (currentSuspicion + 0.1)
          | currentSuspicion > 0.01          = max 0.0 (currentSuspicion - 0.05)
          | otherwise                        = 0.0
    STM.writeTVar (adSuspicionVar detector) newSuspicion
    pure newSuspicion

  let threshold = adcSuspectLevel (adConfig detector)
  pure $ if suspicion >= threshold
         then AdDetected suspicion
         else if suspicion > 0.3
              then AdMonitoring suspicion
              else AdInactive

getAdState :: AdDetector -> IO AdState
getAdState detector = do
  suspicion <- STM.atomically $ STM.readTVar (adSuspicionVar detector)
  let threshold = adcSuspectLevel (adConfig detector)
  pure $ if suspicion >= threshold
         then AdDetected suspicion
         else if suspicion > 0.3
              then AdMonitoring suspicion
              else AdInactive
