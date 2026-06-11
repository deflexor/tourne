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
import Network.HTTP.Client qualified as HC
import Network.HTTP.Simple (parseRequest, setRequestHeader, httpNoBody)
import Network.HTTP.Types.Header (hUserAgent)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Tourne.Error (AppError (..), renderError)
import Tourne.Types
import Tourne.Config

-- | Handle to control the ping checker
data PingHandle = PingHandle
  { phCancel :: !(STM.TVar Bool)
  , phThread :: !(Async ())
  } deriving (Generic)

-- | Start background ping checker.
--
-- Pings are issued in batches of @configPingBatchSize@ stations per cycle,
-- with @configPingIntervalSeconds@ seconds between cycles. A round-robin
-- cursor advances through the station list so that with N stations and
-- batch size B, every station is pinged within roughly N/B * interval
-- seconds. HTTP requests carry a per-request response timeout (from
-- @configPingResponseTimeout@) so a single slow station cannot block the
-- cycle indefinitely.
startPingChecker
  :: Config
  -> STM.TVar [Station]
  -> (StationId -> Either Text Double -> IO ())
  -> IO PingHandle
startPingChecker cfg stationVar onResult = do
  cancelVar <- STM.newTVarIO False
  cursorVar <- STM.newTVarIO 0
  thread <- async $ runPingChecker cancelVar cursorVar cfg stationVar onResult
  pure PingHandle{ phCancel = cancelVar, phThread = thread }

-- | Stop the ping checker
stopPingChecker :: PingHandle -> IO ()
stopPingChecker PingHandle{phCancel, phThread} = do
  STM.atomically $ STM.writeTVar phCancel True
  cancel phThread

-- | Main ping checker loop
runPingChecker
  :: STM.TVar Bool
  -> STM.TVar Int
  -> Config
  -> STM.TVar [Station]
  -> (StationId -> Either Text Double -> IO ())
  -> IO ()
runPingChecker cancelVar cursorVar cfg stationVar onResult = do
  let batchSize      = max 1 (configPingBatchSize cfg)
      intervalMicros = fromIntegral (max 1 (configPingIntervalSeconds cfg)) * 1000000
      -- Connect timeout is not directly settable on a per-request basis
      -- with Network.HTTP.Simple; the shared manager owns it. We rely on
      -- the response timeout below plus the manager's default connect
      -- timeout to bound a single request.
      responseMicros = fromIntegral (max 1 (configPingResponseTimeout cfg)) * 1000000
      loop = do
        cancelled <- STM.atomically $ STM.readTVar cancelVar
        unless cancelled $ do
          stations <- STM.atomically $ STM.readTVar stationVar
          toPing   <- pickBatch batchSize stations cursorVar
          forM_ toPing $ \station -> do
            cancelled' <- STM.atomically $ STM.readTVar cancelVar
            unless cancelled' $ do
              result <- pingStation (toString (stationUrl station)) responseMicros
              -- The onResult callback carries an Either Text Double
              -- result type (not a recoverable error). Convert the
              -- structured AppError to its rendered form here.
              let cbResult = either (Left . renderError) Right result
              onResult (stationId station) cbResult
          -- Sleep in 250ms slices so shutdown is responsive.
          sleepResponsively cancelVar intervalMicros
          loop
  loop

-- | Pick the next batch of stations in round-robin order and advance the
-- cursor. Returns at most @batchSize@ stations; fewer if the list is small.
pickBatch
  :: Int
  -> [Station]
  -> STM.TVar Int
  -> IO [Station]
pickBatch batchSize stations cursorVar = do
  let n = length stations
  if n == 0
    then pure []
    else do
      STM.atomically do
        cursor <- STM.readTVar cursorVar
        let takeN    = min batchSize n
            startIx  = cursor `mod` n
            (before, rest) = splitAt startIx stations
            -- Take from rest first, then wrap to before
            picked   = take takeN (rest <> before)
            nextCur  = (cursor + takeN) `mod` n
        STM.writeTVar cursorVar nextCur
        pure picked

-- | Sleep for the given number of microseconds, but check the cancel flag
-- every 250ms so shutdown is responsive. Returns immediately if cancelled.
sleepResponsively :: STM.TVar Bool -> Int -> IO ()
sleepResponsively cancelVar totalMicros = go totalMicros
  where
    slice = 250000  -- 250ms
    go remaining
      | remaining <= 0 = pure ()
      | otherwise = do
          cancelled <- STM.atomically $ STM.readTVar cancelVar
          if cancelled
            then pure ()
            else do
              threadDelay (min slice remaining)
              go (remaining - slice)

-- | Ping a single station URL. Returns the round-trip time in seconds, or
-- a structured error on failure. The response timeout (in microseconds)
-- bounds the per-request wait.
--
-- Internally returns 'AppError' for the HTTP-failure case; the caller
-- ('runPingChecker') converts to the 'Either Text Double' result
-- expected by the consumer callback (the LHS is a *result* type, not
-- a recoverable error).
pingStation :: String -> Int -> IO (Either AppError Double)
pingStation url responseMicros = do
  result <- try $ do
    start <- getCurrentTime
    request <- parseRequest url
    let req = setRequestHeader hUserAgent ["TourneRadio/0.1.0"] request
          { HC.responseTimeout = HC.responseTimeoutMicro responseMicros
          }
    _response <- httpNoBody req
    end <- getCurrentTime
    let diff = realToFrac (diffUTCTime end start) :: Double
    -- Return ping in seconds
    pure diff
  case result of
    Right ping -> pure (Right ping)
    Left (e :: SomeException) -> pure (Left (HttpError (show e)))
