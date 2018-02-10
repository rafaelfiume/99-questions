module Playground.ListssSpec where

import Test.Hspec (Spec, describe, it, shouldBe, context) -- pending
-- import Test.QuickCheck

import Control.Monad.Writer -- for compressWriter
import Playground.Listss

{-# ANN module "HLint: ignore Use camelCase" #-}

spec_lists :: Spec
spec_lists = describe "Lists (Questions 1 to 10)" $ do
    context "myLast" $
        it "returns the last element" $ do
            myLast [1,2,3,4] `shouldBe` 4
            myLast ['z','x','y'] `shouldBe` 'y'

        -- TODO RF : 03/02/2018 : See how to property check this one
        -- it "returns the last element (property checking)"
        --    pending

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

    context "compress" $ do
        it "replaces with a single copy of the element a list contains repeated elements" $ do
            compress "aaaabccaadeeee" `shouldBe` "abcade"
            compress [1,1,1,5,5,3,3,3,3,3,4] `shouldBe` [1,5,3,4]

        it "(writer monad version) does the same but also logs each step" $ do
            fst (runWriter $ compressWriter "aaaabccaadeeee") `shouldBe` "abcade"
            fst (runWriter $ compressWriter [1,1,1,5,5,3,3,3,3,3,4]) `shouldBe` [1,5,3,4]

    context "pack" $
        it "packs consecutive duplicates of list elements into sublists" $ do
        pack2 ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] `shouldBe` ["aaaa","b","cc","aa","d","eeee"]
        pack2 [1,1,1,2,2,2,2,2,5,3,3,3,3] `shouldBe` [[1,1,1], [2,2,2,2,2], [5], [3,3,3,3]]
