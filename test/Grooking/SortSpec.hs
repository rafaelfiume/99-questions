module Grooking.SortSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

import Grooking.Sort

{-# ANN module "HLint: ignore Use camelCase" #-}

spec_sort :: Spec
spec_sort = describe "Sort" $
        it "sorts a list (Selection Sort)" $ do
            selectionSort [11,2,9,7,10,12,1] `shouldBe` [1,2,7,9,10,11,12]
            selectionSort "mlhyyrhd" `shouldBe` "dhhlmryy"
            selectionSort "a" `shouldBe` "a"
            selectionSort [] `shouldBe` ([] :: [Int])
