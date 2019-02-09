module Util where

import Test.QuickCheck

popAt :: Int -> [a] -> ([a], a)
popAt idx xs = (lh ++ rh, el)
    where (lh, el:rh) = splitAt idx xs

insertAt :: Int -> a -> [a] -> [a]
insertAt idx x list = lh ++ x:rh
    where (lh, rh) = splitAt idx list

prop_insertAt idx x list = (insertAt idx x list) !! idx == x