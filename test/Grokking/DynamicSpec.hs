module Grokking.DynamicSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

import Grokking.Dynamic

spec_dynamic_algorithm :: Spec
spec_dynamic_algorithm =
  describe "Dynamic algorithm" $
    it "should put in the bag the most valuable items" $
      solution `shouldBe` (4000, " iphone laptop")
