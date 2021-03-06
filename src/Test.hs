import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Control.Monad
import Core
import Math.Combinat.Permutations
import Data.Char
import qualified Frontend.AbsTensor as Abs

-- TODO generate nested calcs with sized
instance Arbitrary Calc where
    arbitrary = arbitraryCalc

instance Arbitrary ValenceType where
    arbitrary = oneof $ map return [Up, Down]

-- beware! will generate bogus contractions due to indices not being known in
-- this context
-- instance Arbitrary Operation where
    -- arbitrary = do
        -- p <- arbitrary :: Gen (Positive Int)
        -- id1 <- arbitrarySizedNatural
        -- id2 <- arbitrarySizedNatural
        -- oneof $ map return [(:+), (:*), Power $ getPositive p, Contract id1 id2]

instance Arbitrary ReprType where
    arbitrary = do
        n <- arbitrary :: Gen (Positive Int)
        return $ ReprType $ getPositive n

instance Arbitrary Index where
    arbitrary = Index <$> arbitrary <*> arbitrary

arbitraryCalc :: Gen Calc
arbitraryCalc = oneof [
    Number <$> arbitrary,
    arbitraryTensor,
    arbitrarySum,
    arbitraryProduct
 ]

-- constructs a self contracted tensor
arbitrarySelfContraction :: Gen Calc
arbitrarySelfContraction = do
    t <- suchThat arbitraryTensor (\(Tensor _ xs) -> (length xs) >= 2)
    case t of
        (Tensor n ids) -> do 
            cid <- (arbitrary :: Gen Index)
            slot1 <- choose (0, length ids - 1)
            slot2 <- suchThat (choose (0, length ids - 1)) (not . (==) slot1)
            let cids = if slot1 > slot2
                then (insertAt slot1 cid . insertAt slot2 (flipIndex cid)) ids
                else (insertAt slot2 cid . insertAt slot1 (flipIndex cid)) ids
            return $ (Contract slot1 slot2) $ Tensor n cids
                where flipIndex i@(Index r v) = i { indexValence = flipValence v }
                      flipValence Up = Down
                      flipValence Down = Up
        _ -> undefined

-- inserts an element at position idx counted from 0
insertAt :: Int -> a -> [a] -> [a]
insertAt idx x xs = lh ++ x:rh
    where (lh, rh) = splitAt idx xs


-- TODO: generate linearly dependent but not identical vector spaces
arbitrarySum :: Gen Calc
arbitrarySum = do
    tensor <- arbitraryTensor
    return $ tensor |+| tensor

arbitraryProduct :: Gen Calc
arbitraryProduct = do
    tensor1 <- oneof [arbitraryTensor, arbitrarySelfContraction]
    tensor2 <- oneof [arbitraryTensor, arbitrarySelfContraction]
    return $ tensor1 |*| tensor2

-- only generates identity permutations
arbitraryTensor :: Gen Calc
arbitraryTensor = do
    indices <- listOf arbitrary
    name <- arbitraryIdentifier
    return $ Tensor name indices

-- only generates parsable identifiers
arbitraryIdentifier :: Gen String
arbitraryIdentifier = vectorOf 2 (elements ['a'..'z']) >>= \(x:xs) -> return $ toUpper x : xs
--arbitraryIdentifier = listOf1 (elements ['a'..'z']) >>= \xs -> sized (\n -> return $ take n xs)


sort :: Ord a => [a] -> [a]
sort [] = []
sort (pivot:rest) = sort (filter ((>) pivot) rest) ++ [pivot] ++ sort (filter ((<=) pivot) rest)

prop_sortPairwiseAsc list = isSorted sorted
    where sorted = sort list
          isSorted ([]) = True
          isSorted (x:[]) = True
          isSorted (x:y:xs) = (x <= y) && isSorted (y:xs)
          types = list :: [Int]

prop_sortInvariantLength list = length (sort list) == length list
    where types = list :: [Int]

instance Arbitrary Abs.Expr where
    arbitrary = genericArbitrary

instance Arbitrary Abs.Label where
    arbitrary = genericArbitrary

instance Arbitrary Abs.Index where
    arbitrary = genericArbitrary