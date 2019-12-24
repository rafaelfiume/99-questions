module Grokking.BreadthFirst (
  bdSearch
) where

import qualified Data.HashMap.Strict as Map

-- Source: https://github.com/egonSchiele/grokking_algorithms/blob/master/06_breadth-first_search/Haskell/01_bfs.hs

-- the graph
users =
  Map.fromList
    [ ("you", ["alice", "bob", "claire"])
    , ("bob", ["anuj", "peggy"])
    , ("alice", ["peggy"])
    , ("claire", ["mangoes", "jonny"])
    , ("anuj", [])
    , ("peggy", [])
    , ("mangoes", [])
    , ("johny", [])
    ]

connections :: String -> Maybe [String]
connections name = Map.lookup name users

bdSearch :: Maybe [String] -> [String] -> Maybe String
bdSearch toBeSearched searched = case toBeSearched of
  Just (x:xs)
    | x == "mangoes" -> Just x
    | x `elem` searched -> bdSearch (Just xs) searched
    | otherwise -> bdSearch ((++) <$> Just xs <*> connections x) (x : searched)
  Nothing -> Nothing
