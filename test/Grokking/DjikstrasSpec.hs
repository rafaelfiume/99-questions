module Grokking.DjikstrasSpec where

-- Source : https://github.com/egonSchiele/grokking_algorithms/blob/master/07_dijkstras_algorithm/Haskell/01_dijkstras_algorithm.hs

import Grokking.Djikstras

import qualified Data.HashMap.Strict as Map

import Test.Hspec (Spec, describe, it, shouldBe)

{-# ANN module "HLint: ignore Use camelCase" #-}

graph =
  Map.fromList
    [ ("book", [("lp", 5), ("poster", 0)])
    , ("lp", [("guitar", 15), ("drums", 20)])
    , ("poster", [("guitar", 30), ("drums", 35)])
    , ("guitar", [("piano", 20)])
    , ("drums", [("piano", 10)])
    , ("piano", [])
    ]

buildMap :: Graph -> a -> (String -> Map.HashMap String a) -> String -> Map.HashMap String a
buildMap graph def initMapFn node = foldl
  (\accMap key -> Map.insert key def accMap)
  startingMap
  keysToAdd
  where startingMap = initMapFn node
        startKeys = node : Map.keys startingMap
        allKeys = Map.keys graph
        keysToAdd = filter (not . (`elem` startKeys)) allKeys

initCosts node = buildMap graph infinity (neighbors graph) node

initParents node = buildMap graph "" (Map.map (const node) . neighbors graph) node

spec_djikstras :: Spec
spec_djikstras = describe "Djikstra's algorithm" $
  it "returns the path with the lowest cost" $ do
    let costs = initCosts "book"
    let parents = initParents "book"
    let processed = ["book"]
    let (costSolution, parentSolution) = shortestPath graph costs parents processed
    costTo "piano" costSolution `shouldBe` 35.0
    pathTo "piano" parentSolution `shouldBe` ["book", "lp", "drums", "piano"]
