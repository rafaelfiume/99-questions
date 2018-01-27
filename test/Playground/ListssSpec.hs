module Playground.ListssSpec where

import Test.Hspec (Spec, describe, it, shouldBe, pending, context)
import Control.Exception (evaluate)
import Test.QuickCheck

import Playground.Listss

{-# ANN module "HLint: ignore Use camelCase" #-}

spec_preludeExample :: Spec
spec_preludeExample = describe "Listss stuff" $ do
    context "myLast" $ do
        it "returns the last element" $ do
            myLast [1,2,3,4] `shouldBe` 4
            myLast ['z','x','y'] `shouldBe` 'y'

        it "returns the last element (property checking)"
            pending

    context "myButLast" $
        it "returns the last but one element of a list" $
            myButLast [1,2,3,4] `shouldBe` 3

    context "elementAt" $
        it "returns the K'th element of a list (first element in the list is number 1)" $ do
            elementAt [1,2,3,4] 2 `shouldBe` 2
            elementAt "haskell" 5 `shouldBe` 'e'

    context "myLength" $
        it "returns the number of elements of a list" $ do
            myLength [123, 456, 789] `shouldBe` 3
            myLength "Hello, world!" `shouldBe` 13

    context "myReverse" $
        it "reverses a list" $ do
            myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"
            myReverse [1,2,3,4] `shouldBe` [4,3,2,1]

    context "myPalindrome" $
        it "finds out whether a list is a palindrome" $ do
            isPalindrome "madamimadam" `shouldBe` True
            isPalindrome [1,2,4,8,16,8,4,2,1] `shouldBe` True
            isPalindrome "Joana" `shouldBe` False

    context "flatten" $
        it "flattens a nested list" $ do
            flatten (Elem 5) `shouldBe` [5]
            flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1,2,3,4,5]
            flatten (List []) `shouldBe` ([] :: [Int]) -- Cast is necessary otherwise compiler complains "wha's the f#*k1n3 type?"
