module Grokking.Djikstras (
  shortestPath,
  costTo,
  pathTo,
  Graph,
  Costs,
  Parents,
  infinity,
  neighbors
)where

-- Source : https://github.com/egonSchiele/grokking_algorithms/blob/master/07_dijkstras_algorithm/Haskell/01_dijkstras_algorithm.hs

import Control.Applicative
import Data.Maybe
import Data.List
import qualified Data.HashMap.Strict as Map

type Graph = Map.HashMap String [WeightedEdge]
type Costs = Map.HashMap String Double
type Parents = Map.HashMap String String
type WeightedEdge = (String, Double)

infinity = read "Infinity" :: Double

neighbors :: Graph -> String -> Costs
neighbors graph node = Map.fromList (fromMaybe [] (Map.lookup node graph))

closest :: Graph -> String -> WeightedEdge
closest graph node = minimumBy (\x y -> compare (snd x) (snd y)) $ Map.toList (neighbors graph node)

safeHead [] = Nothing
safeHead (x:xs) = Just x

cheapest :: [String] -> Costs -> Maybe WeightedEdge
cheapest processed costs =
  safeHead $
  sortBy (\x y -> compare (snd x) (snd y)) $
  filter (\(a, b) -> (not . (`elem` processed)) a) $
  Map.toList costs

updateCosts :: Graph -> Costs -> WeightedEdge -> Costs
updateCosts graph costs (node, cost) =
  foldl
    (\acc (neighbor, neighborCost) ->
       let (Just newCost) = min (neighborCost + cost) <$> Map.lookup neighbor acc
        in Map.insert neighbor newCost acc)
    costs
    edges
  where
    edges = Map.toList $ neighbors graph node

updateParents :: Graph -> Parents -> Costs -> WeightedEdge -> Parents
updateParents graph parents costs (node, cost) = foldl
   (\acc (neighbor, neighborCost) ->
     case ((cost + neighborCost) <) <$> Map.lookup neighbor costs of
       Just True -> Map.insert neighbor node acc
       _ -> acc)
  parents
  edges
  where edges = Map.toList $ neighbors graph node

shortestPath :: Graph -> Costs -> Parents -> [String] -> (Costs, Parents)
shortestPath graph costs parents processed =
    case cheapest processed costs of
    Just (node, cost) -> shortestPath graph newCosts newParents (node : processed)
      where newCosts = updateCosts graph costs (node, cost)
            newParents = updateParents graph parents costs (node, cost)
    Nothing -> (costs, parents)

costTo :: String -> Costs -> Double
costTo node costMap =
  case Map.lookup node costMap of
    Just cost -> cost
    _ -> infinity

pathTo :: String -> Parents -> [String]
pathTo node parents = buildPath node parents [node]
  where buildPath node parents acc = case Map.lookup node parents of
          Just "book" -> "book" : acc
          Just parent -> buildPath parent parents (parent : acc)
          Nothing -> acc
