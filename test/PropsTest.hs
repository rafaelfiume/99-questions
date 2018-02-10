module PropsTest where

-- import Test.QuickCheck

import Playground.Listss (myReverse, myLast)

--prop_ex1 b = b == False
--    where types = (b :: Bool)

--prop_ex2 i = i == 42
--    where types = (i :: Int)

prop_RevRev xs = myReverse (myReverse xs) == xs
    where types = xs :: [Int]

prop_Last xs = myLast xs == (head . reverse) xs
    where types = xs :: [Int]
