module Grooking.BinarySearch (
    binarySearch
) where

binarySearch :: (Eq a, Ord a) => [a] -> a -> Maybe Int
binarySearch xs e = doBinarySearch xs e 0 (length xs - 1)

doBinarySearch :: (Eq a, Ord a) => [a] -> a -> Int -> Int -> Maybe Int
doBinarySearch [] _ _ _ = Nothing
doBinarySearch xs e l h
    | e == middleElem = Just middleIndex
    | e < middleElem = doBinarySearch xs e l (middleIndex - 1)
    | e > middleElem = doBinarySearch xs e (l + 1) h
        where middleIndex = (l + h) `div` 2
              middleElem = xs !! middleIndex
