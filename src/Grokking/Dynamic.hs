module Grokking.Dynamic (
  solution
)where

import qualified Data.HashMap.Strict as Map
import Data.Array

-- Source: https://github.com/egonSchiele/grokking_algorithms/blob/master/09_dynamic_programming/Haskell/01_knapsack_dynamic_prog.hs

type Grid = Array (Integer, Integer)(Integer, String)

itemsMap = Map.fromList [
  ("stereo", (4, 3000)),
  ("laptop", (3, 2000)),
  ("guitar", (1, 1500)),
  ("iphone", (1, 2000))
  ]

weightOf item = case Map.lookup item itemsMap of
  Just (w, _) -> w
  _ -> 0

valueOf item = case Map.lookup item itemsMap of
  Just (_, v) -> v
  _ -> 0

emptyGrid :: Grid
emptyGrid = array ((0, 0), (3, 4)) [((x, y), (0, "")) | x <- [0..3], y <- [0..4]]

best :: Grid -> Integer -> Integer -> String -> (Integer, String)
best arr row col item =
  let weight = weightOf item
      value = valueOf item
      (previousMax, _) = if row /= 0 then arr ! (row - 1, col) else (0, "")
      (valueOfRemainingSpace, itemsInRemainingSpace) =
        if row /= 0 && (col - weight) >= 0
        then arr ! (row - 1, col - weight)
        else (0, "")
  in if previousMax > (value + valueOfRemainingSpace)
    then arr ! (row - 1, col)
    else (value + valueOfRemainingSpace, itemsInRemainingSpace ++ " " ++ item)

fillPrevBest :: Grid -> Integer -> Integer -> Grid
fillPrevBest arr row col =
  if row /=0 then arr // [((row, col), arr ! (row - 1, col))] else arr

fillGrid :: Grid -> Grid
fillGrid emptyGrid = foldl
  (\acc pair ->
    let row = fst pair
        item = snd pair
        (weight, value) = (weightOf item, valueOf item)
    in foldl
       (\arr col ->
         if weight <= col
         then arr // [((row, col), best arr row col item)]
         else fillPrevBest arr row col
       )
       acc
       [0..4]
    )
  emptyGrid
  items
  where items = zip [0..3] $ Map.keys itemsMap

solution = foldl
  (\(x, a) (y, b) -> if x > y then (x, a) else (y, b))
  first
  rest
  where (first : rest) = elems $ fillGrid emptyGrid
