{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Playground.Listss (
    myLast,
    myButLast,
    elementAt,
    myLength,
    myReverse,
    isPalindrome,
    NestedList(..), flatten,
    compress, compressWriter,
    pack2
) where

import Control.Monad (foldM)               -- for compressWriter (8th)
import Control.Monad.Writer (Writer, tell) -- for compressWriter (8th)

import Data.List (group, foldl')           -- group is for pack (9th)

-- 1st

myLast :: [a] -> a
myLast [] = error "no last element in a empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' = foldr1 (const id)

-- 2nd

myButLast :: [a] -> a
myButLast [] = error "no but last element in an empty list"
myButLast [_] = error "just one element in the list"
myButLast xs = let (_:xy) = reverse xs in head xy

-- 3rd

elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list"
elementAt (x:_) 1 = x
elementAt (_:xs) k
    | k < 1 = error "index must be equal or greater than one"
    | otherwise = elementAt xs (k-1)

-- 4th

myLength :: [a] -> Int
myLength = foldl' (\x _ -> x + 1) 0

-- 5th

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl' (flip (:)) []

-- 6th

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [_] = True
isPalindrome' xs = head xs == last xs && isPalindrome' (tail . init $ xs)

-- 7th

data NestedList a = Elem a | List [NestedList a] deriving (Show)

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List ls) = concatMap flatten' ls

-- 8th

compress :: (Eq a) => [a] -> [a]
compress = reverse . foldl' addIfNotThere []
    where addIfNotThere []  x = [x]
          addIfNotThere acc x
                        | head acc == x = acc
                        | otherwise     = x:acc

-- This is a shorter but slightly slower version
compress2 :: (Eq a) => [a] -> [a]
compress2 = foldr addIfNotThere []
    where addIfNotThere x [] = [x]
          addIfNotThere x acc
                        | head acc == x = acc
                        | otherwise     = x:acc

-- mapM_ putStrLn $ snd $ runWriter (compressWriter "aaaabccaadeeee")
compressWriter :: (Eq a, Show a) => [a] -> Writer [String] [a]
compressWriter xs = foldM skipSame [] $ reverse xs -- we need to reverse since we're going from left to right

skipSame :: (Eq a, Show a) => [a] -> a -> Writer [String] [a]
skipSame [] x = do
    tell ["x is " ++ show x ++ "; list is []"]
    return [x]
skipSame acc x = do
    let current = if head acc == x then acc else x:acc
    tell ["x is " ++ show x ++ "; list is " ++ show current]
    return current

compress' :: (Eq a) => [a] -> [a]
compress' xs = foldr f (const []) xs Nothing
    where f x r a@(Just q) | x == q = r a
          f x r _ = x : r (Just x)

-- 9th

pack :: (Eq a) => [a] -> [[a]]
pack = group

pack2 :: (Eq a) => [a] -> [[a]]
pack2 = foldr packSame []
    where packSame e (x:xs) | head x == e = (e:x):xs
          packSame e xs = [e]:xs
