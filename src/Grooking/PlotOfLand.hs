module Grooking.PlotOfLand(
    splitUp
) where

splitUp :: Int -> Int -> Int
splitUp l w
    | l == 2 * w = w
    | l * 2 == w = l
    | l > w   = splitUp (l - w) w
    | l < w   = splitUp l (w - l)
