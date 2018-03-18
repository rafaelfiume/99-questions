module Grooking.SortSpec where

import Test.Hspec (Spec, describe, context, it, shouldBe)
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Grooking.Sort

{-# ANN module "HLint: ignore Use camelCase" #-}

spec_sort :: Spec
spec_sort = describe "Sort" $ do
    context "selectionSort" $ do
        it "sorts a list" $ do
            selectionSort [] `shouldBe` ([] :: [Int])
            selectionSort "a" `shouldBe` "a"
            selectionSort [11,2,9,7,10,12,1] `shouldBe` [1,2,7,9,10,11,12]
            selectionSort "mlhyyrhd" `shouldBe` "dhhlmryy"

        prop "first element is always the smallest" $
            \xs -> not (null xs) ==> head (selectionSort xs) == minimum (xs :: String)

    context "quicksort" $ do
        it "sorts a list" $ do
            quicksort [] `shouldBe` ([] :: [Int])
            quicksort "a" `shouldBe` "a"
            quicksort [11,2,9,7,10,9,12,1] `shouldBe` [1,2,7,9,9,10,11,12]
            quicksort "mlhyyrhd" `shouldBe` "dhhlmryy"

        prop "first element is always the smallest" $
            \xs -> not (null xs) ==> head (selectionSort xs) == minimum (xs :: String)
