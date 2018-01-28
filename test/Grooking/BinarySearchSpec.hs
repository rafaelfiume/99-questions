module Grooking.BinarySearchSpec where

import Test.Hspec (Spec, describe, it, shouldBe, context)

import Grooking.BinarySearch

{-# ANN module "HLint: ignore Use camelCase" #-}

spec_binarySearch :: Spec
spec_binarySearch = describe "Binary Search" $
    it "returns the index of the element present in a list" $ do
        binarySearch [1,2,3,4,5,6,7,8,9,10,11] 8 `shouldBe` Just 7
        binarySearch "abcdeopqerst" 't' `shouldBe` Just 11
        binarySearch "abc" 'a' `shouldBe` Just 0
        binarySearch [] 8 `shouldBe` Nothing
