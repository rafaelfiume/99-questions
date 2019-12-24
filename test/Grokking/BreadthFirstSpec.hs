module Grokking.BreadthFirstSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

import Grokking.BreadthFirst

{-# ANN module "HLint: ignore Use camelCase" #-}

spec_breadth_first :: Spec
spec_breadth_first =
  describe "Breadth-first algorithm" $
    it "returns the nearest path to the object of search" $
      bdSearch (Just ["you"]) [] `shouldBe` Just "mangoes"