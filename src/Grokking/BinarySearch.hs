module Grokking.BinarySearch (
    binarySearch
) where

binarySearch :: (Ord a) => [a] -> a -> Maybe Int
binarySearch xs e = doBinarySearch xs e low high
    where low  = 0
          high = length xs - 1

doBinarySearch :: (Ord a) => [a] -> a -> Int -> Int -> Maybe Int
doBinarySearch [] _ _ _ = Nothing
doBinarySearch xs e l h
    | e == middleElem = Just middleIndex
    | e < middleElem  = doBinarySearch xs e l (middleIndex - 1)
    | e > middleElem  = doBinarySearch xs e (middleIndex + 1) h
        where middleIndex = (l + h) `div` 2
              middleElem  = xs !! middleIndex -- TODO There is a big efficiency issue here...
                                              -- !! will go through the list till it finds the element in question
                                              -- Consider to use Data.Array (!) instead
