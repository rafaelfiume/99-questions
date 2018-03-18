module Lib
    ( someFunc
    ) where

import Playground.Listss

someFunc :: IO ()
someFunc = do
    putStrLn "bla bla bla"
    putStrLn $ myReverse "rafael fiume"
