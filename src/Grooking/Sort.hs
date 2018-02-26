module Grooking.Sort (
    selectionSort
) where

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = smaller : selectionSort remaining
    where (smaller, remaining) = whoIsTheMinimum xs

whoIsTheMinimum :: (Ord a) => [a] -> (a, [a])
whoIsTheMinimum (x:xs) = foldl theSmallerOne (x, []) xs

theSmallerOne :: (Ord a) => (a, [a]) -> a -> (a, [a])
theSmallerOne (prev, xs) e
    | prev <= e = (prev, e:xs)
    | otherwise = (e, prev:xs)
