module Util where

import Test.QuickCheck
import Math.Combinat.Permutations
import Data.List
import Debug.Trace


deleteAt :: Show a => Int -> [a] -> [a]
deleteAt idx l = lh ++ rh
    where (lh,_:rh) = splitAt (idx) (l)

popAt :: Int -> [a] -> ([a], a)
popAt idx xs = (lh ++ rh, el)
    where (lh, el:rh) = splitAt idx xs

-- inserts an element at position idx counted from 0
insertAt :: Int -> a -> [a] -> [a]
insertAt idx x xs = lh ++ x:rh
    where (lh, rh) = splitAt idx xs

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx x xs = lh ++ x:rh
    where (lh, _:rh) = splitAt idx xs

transformAt :: Int -> (a -> a) -> [a] -> [a]
transformAt idx f xs = lh ++ f r : rh
    where (lh, r:rh) = splitAt idx xs

image :: Permutation -> Int -> Int
image perm i = unsafeMaybe $ elemIndex i $ permuteList perm [0..]

unsafeElemIndex i = unsafeMaybe . elemIndex i

unsafeMaybe :: Maybe a -> a
unsafeMaybe (Just x) = x
unsafeMaybe _ = undefined

unsafeEither :: Either a b -> b
unsafeEither (Right b) = b
unsafeEither (Left a) = undefined