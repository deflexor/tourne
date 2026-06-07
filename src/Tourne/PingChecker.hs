module Tourne.PingChecker
  ( startPingChecker
  , stopPingChecker
  , PingHandle
  ) where

import Relude
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Exception (try)
import Control.Concurrent.STM qualified as STM
import Network.HTTP.Simple (parseRequest, setRequestHeader, httpNoBody)
import Network.HTTP.Types.Header (hUserAgent)
import Data.Time.Clock (NominalDiffTime, getCurrentTime, diffUTCTime)
import Data.ByteString qualified as BS
import Tourne.Types
import Tourne.Config

-- | Handle to control the ping checker
data PingHandle = PingHandle
  { phCancel   :: !(STM.TVar Bool)
  , phThread   :: !(Async ())
  } deriving (Generic)

-- | Start background ping checker
startPingChecker :: Config -> STM.TVar [Station] -> (StationId -> Either Text Double -> IO ()) -> IO PingHandle
startPingChecker cfg stationVar onResult = do
  cancelVar <- STM.newTVarIO False
  thread <- async $ runPingChecker cancelVar cfg stationVar onResult
  pure PingHandle{ phCancel = cancelVar, phThread = thread }

-- | Stop the ping checker
stopPingChecker :: PingHandle -> IO ()
stopPingChecker PingHandle{phCancel, phThread} = do
  STM.atomically $ STM.writeTVar phCancel True
  cancel phThread

-- | Main ping checker loop
runPingChecker
  :: STM.TVar Bool
  -> Config
  -> STM.TVar [Station]
  -> (StationId -> Either Text Double -> IO ())
  -> IO ()
runPingChecker cancelVar cfg stationVar onResult = do
  let loop = do
        cancelled <- STM.atomically $ STM.readTVar cancelVar
        unless cancelled $ do
          stations <- STM.atomically $ STM.readTVar stationVar
          -- Ping up to 10 stations per cycle
          let toPing = take 10 stations
          forM_ toPing $ \station -> do
            cancelled' <- STM.atomically $ STM.readTVar cancelVar
            unless cancelled' $ do
              pingStation station >>= onResult (stationId station)
          threadDelay (30 * 1000000)  -- 30 seconds between cycles
          loop
  loop

-- | Ping a single station
pingStation :: Station -> IO (Either Text Double)
pingStation station = do
  let url = stationUrl station
  result <- try $ do
    start <- getCurrentTime
    request <- parseRequest (toString url)
    let req = setRequestHeader hUserAgent ["TourneRadio/0.1.0"] request
    _response <- httpNoBody req
    end <- getCurrentTime
    let diff = realToFrac (diffUTCTime end start) :: Double
    -- Return ping in seconds
    pure diff
  case result of
    Right ping -> pure (Right ping)
    Left (e :: SomeException) -> pure (Left $ show e)
