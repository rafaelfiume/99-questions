module Lib
    ( someFunc
    ) where

import Playground.Learning

someFunc :: IO ()
someFunc = do
    putStrLn "bla bla bla"
    putStrLn $ reverse' "rafael fiume"
