module Playground.Listss (
    myLast,
    myButLast,
    elementAt,
    myLength,
    myReverse,
    isPalindrome,
    flatten,
    NestedList(..)
) where

myLast :: [a] -> a
myLast [] = error "no last element in a empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' = foldr1 (const id)

myButLast :: [a] -> a
myButLast [] = error "no but last element in an empty list"
myButLast [x] = error "just one element in the list"
myButLast xs = let (x:xy) = reverse xs in head xy

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) k
    | k < 1 = error "index can't be lesser than one"
    | otherwise = elementAt xs (k-1)

myLength :: [a] -> Int
myLength = foldl (\x _ -> x + 1) 0

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

anotherReverse :: [a] -> [a]
anotherReverse = foldl (flip (:)) []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [_] = True
isPalindrome' xs = head xs == last xs && isPalindrome' (tail . init $ xs)

data NestedList a = Elem a | List [NestedList a] deriving (Show)

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
