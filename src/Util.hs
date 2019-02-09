module Util where

import Test.QuickCheck

deleteAt :: Int -> [a] -> [a]
deleteAt idx l = lh ++ rh
    where (lh,_:rh) = splitAt idx l

popAt :: Int -> [a] -> ([a], a)
popAt idx xs = (lh ++ rh, el)
    where (lh, el:rh) = splitAt idx xs

insertAt :: Int -> a -> [a] -> [a]
insertAt idx x list = lh ++ x:rh
    where (lh, rh) = splitAt idx list

prop_insertAt idx x list = (insertAt idx x list) !! idx == x