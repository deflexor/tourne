module Tourne.Failover
  ( FailoverEngine
  , initFailover
  , checkFailover
  , findSimilarStation
  ) where

import Relude
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Tourne.Types
import Tourne.Config

--------------------------------------------------------------------------------
-- Failover engine
--------------------------------------------------------------------------------

data FailoverEngine = FailoverEngine
  { feConfig :: !Config
  } deriving (Generic)

initFailover :: Config -> FailoverEngine
initFailover cfg = FailoverEngine
  { feConfig = cfg
  }

--------------------------------------------------------------------------------
-- Find similar station for failover
--------------------------------------------------------------------------------

findSimilarStation :: Config -> Station -> [Station] -> HashMap Text Double -> Maybe Station
findSimilarStation cfg currentStation candidates pingResults =
  let
    others = filter (\s -> stationId s /= stationId currentStation) candidates
    currentTags    = stationTags currentStation
    currentBitrate = stationBitrate currentStation

    scored = map (\s ->
      let
        sharedTags = length $ filter (`elem` stationTags s) currentTags
        tagScore   = if null currentTags then 0
                     else fromIntegral sharedTags / fromIntegral (length currentTags)
        bitrateScore = case (currentBitrate, stationBitrate s) of
          (Just cb, Just sb) ->
            1.0 - abs (fromIntegral cb - fromIntegral sb) / max (fromIntegral cb) (fromIntegral sb)
          _ -> 0.5
        pingScore = case HashMap.lookup (unStationId (stationId s)) pingResults of
          Just ping -> max 0.0 (1.0 - ping / 5.0)
          Nothing   -> 0.3
        totalScore = tagScore * 0.5 + bitrateScore * 0.3 + pingScore * 0.2
      in (s, totalScore)) others

    sorted = List.sortBy (\(_, s1) (_, s2) -> compare s2 s1) scored
  in
    case sorted of
      []             -> Nothing
      ((best, _) : _) -> Just best

--------------------------------------------------------------------------------
-- Failover check
--------------------------------------------------------------------------------

checkFailover :: FailoverEngine -> StreamHealth -> Maybe Station -> Maybe StationId -> Maybe StationId
checkFailover _engine health maybeBestStation currentStationId = case health of
  StreamGood        -> Nothing
  StreamDegraded _  -> case maybeBestStation of
    Just _  -> currentStationId
    Nothing -> Nothing
  StreamLost _ -> case maybeBestStation of
    Just best -> Just (stationId best)
    Nothing   -> Nothing
