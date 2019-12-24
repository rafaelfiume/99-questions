module Grokking.GreedySpec where

import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set

import Test.Hspec (Spec, describe, it, shouldBe)

import Grokking.Greedy

stations :: Stations
stations =
  Map.fromList
    [ ("kone", Set.fromList ["id", "nv", "ut"])
    , ("ktwo", Set.fromList ["wa", "id", "mt"])
    , ("kthree", Set.fromList ["or", "nv", "ca"])
    , ("kfour", Set.fromList ["nv", "ut"])
    , ("kfive", Set.fromList ["ca", "az"])
    ]

statesNeeded = Set.fromList ["mt", "wa", "or", "id", "nv", "ut", "ca", "az"]

spec_greedy_algorithm :: Spec
spec_greedy_algorithm =
  describe "Greedy algorith" $
    it "should pick up the optimal solution" $
      stationSet stations statesNeeded [] `shouldBe` ["kfive","kone","kthree","ktwo"]
