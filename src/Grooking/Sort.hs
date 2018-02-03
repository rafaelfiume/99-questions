module Grooking.Sort where

--selectionSort :: (Ord a) => [a] -> [a]
--selectionSort xs = sortThatOut xs []
--    where sortThatOut [] sorted = sorted
--          sortThatOut uns sort = let (small, remaining) = smaller uns in sortThatOut remaining (sort ++ [small])

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = min : selectionSort remaining
    where (min, remaining) = whoIsTheMinimum xs

whoIsTheMinimum :: (Ord a) => [a] -> (a, [a])
whoIsTheMinimum (x:xs) = foldl theSmallerOne (x, []) xs

theSmallerOne :: (Ord a) => (a, [a]) -> a -> (a, [a])
theSmallerOne (prev, xs) e
    | prev <= e = (prev, e:xs)
    | otherwise = (e, prev:xs)
