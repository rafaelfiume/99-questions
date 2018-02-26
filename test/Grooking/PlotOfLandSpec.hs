module Grooking.PlotOfLandSpec where

import Test.Hspec (Spec, describe, it, shouldBe, context)

import Grooking.PlotOfLand

{-# ANN module "HLint: ignore Use camelCase" #-}

spec_divideAndConquer :: Spec
spec_divideAndConquer = describe "Using Euclid's algorithm to divide a farm evenly into square plots" $
    context "splitUp" $ do
        it "finds the largest square box that can be used (base case)" $ do
            splitUp 50 25 `shouldBe` 25
            splitUp 25 50 `shouldBe` 25

        it "finds the largest square box that can be used" $ do
            splitUp 1680 640 `shouldBe` 80
