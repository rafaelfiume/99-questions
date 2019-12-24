-- Use pattern type signature, like f (xs::[b]) = reverse xs, to simplify  property-based testing with Hspec.
-- For more information, go to https://downloads.haskell.org/~ghc/5.00/docs/set/scoped-type-variables.html.
{-# LANGUAGE ScopedTypeVariables #-}

module Grokking.SortSpec where

import Test.Hspec (Spec, describe, context, it, shouldBe)
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Grokking.Sort

import Playground.Listss (myLast)
import Data.List ((\\))

{-# ANN module "HLint: ignore Use camelCase" #-}

spec_sort :: Spec
spec_sort = describe "Sort" $ do
    context "selectionSort" $ do
        it "sorts a list" $ do
            selectionSort [] `shouldBe` ([] :: [Int])
            selectionSort "a" `shouldBe` "a"
            selectionSort [11,2,9,7,10,12,1] `shouldBe` [1,2,7,9,10,11,12]
            selectionSort "mlhyyrhd" `shouldBe` "dhhlmryy"

        prop "first element is always the smallest" $ \(xs :: [Int]) ->
            not (null xs) ==>
                head (selectionSort xs) == minimum xs

    context "quicksort" $ do
        it "sorts a list" $ do
            quicksort [] `shouldBe` ([] :: [Int])
            quicksort "a" `shouldBe` "a"
            quicksort [11,2,9,7,10,9,12,1] `shouldBe` [1,2,7,9,9,10,11,12]
            quicksort "mlhyyrhd" `shouldBe` "dhhlmryy"

        modifyMaxSize (const 1000) $ do
            prop "first sorted element is the smallest one" $ \(xs :: String) ->
                not (null xs) ==>
                    head (quicksort xs) == minimum xs

            prop "last sorted element is the largest one" $ \(xs :: [Int]) ->
                not (null xs) ==>
                    myLast (quicksort xs) == maximum xs

            prop "output is a permutation of input" $ \(xs :: [Int]) ->
                let permutation ys zs = null (ys \\ zs) && null (zs \\ ys)
                in permutation (quicksort xs) xs

            prop "the smaller element of two lists is the first element of those two lists combined and sorted" $ \(xs :: [Int]) (ys :: [Int]) ->
                not (null xs) ==>
                not (null ys) ==>
                    head (quicksort (xs ++ ys)) == min (minimum xs) (minimum ys)

            prop "the output is ordered" $ \(xs :: [Int]) ->
                ordered (quicksort xs)
                    where ordered []       = True
                          ordered [_]      = True
                          ordered (x:y:zs) = x <= y && ordered (y:zs)
