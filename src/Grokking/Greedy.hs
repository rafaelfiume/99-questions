module Grokking.Greedy (
  stationSet,
  Stations
)where

-- Source: https://github.com/egonSchiele/grokking_algorithms/blob/master/08_greedy_algorithms/Haskell/01_set_convering.hs

import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set

type States = Set.Set String
type Station = String
type StationCoverage = (Station, States)
type Stations = Map.HashMap Station States

bestStation :: States -> [Station] -> Stations -> StationCoverage
bestStation statesNeeded selectedStations stations = foldl
  (\a@(station1, states1) b@(station2, states2) ->
    let fn states = Set.size $ Set.intersection statesNeeded states
        coverage1 = fn states1
        coverage2 = fn states2
    in if coverage1 > coverage2 then a else b
  )
  x
  xs
  where (x : xs) = filter (\(station, _) -> station `notElem` selectedStations) $ Map.toList stations

stationSet :: Stations -> States -> [Station] -> [Station]
stationSet allStations statesNeeded finalStations =
  let (station, coveredStates) = bestStation statesNeeded finalStations allStations
      neededStates = Set.difference statesNeeded coveredStates
      newStations = station : finalStations
  in if Set.size statesNeeded == 0 then finalStations else stationSet allStations neededStates newStations
