module Grokking.PlotOfLand(
    splitUp
) where

splitUp :: Int -> Int -> Int
splitUp w l
  | w `mod` l == 0 = l
  | l `mod` w == 0 = w
  | w >= l          = splitUp (w `mod` l) l
  | l > w           = splitUp w (l `mod` w)

