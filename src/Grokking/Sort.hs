module Grokking.Sort (
    selectionSort,
    quicksort
) where

import Data.List

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

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort xs = quicksort lower ++ pivots ++ quicksort higher
    where pivot  = xs !! (length xs `div` 2) -- Ineficient use of (!!) Use Array instead
          lower  = filter (< pivot) xs
          higher = filter (> pivot) xs
          pivots = filter (== pivot) xs
