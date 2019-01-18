import Test.QuickCheck
import Control.Monad
import Core
import Math.Combinat.Permutations
import Data.Char

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
    t@(Tensor n ids perm) <- suchThat arbitraryTensor (\(Tensor _ xs _) -> (length xs) >= 2)
    cid <- arbitrary :: Gen Index
    slot1 <- choose (0, length ids - 1)
    slot2 <- suchThat (choose (0, length ids - 1)) (not . (==) slot1)
    let cids = if slot1 > slot2
        then (insertAt slot1 cid . insertAt slot2 (flipIndex cid)) ids
        else (insertAt slot2 cid . insertAt slot1 (flipIndex cid)) ids
    return $ Op (Contract slot1 slot2) [Tensor n cids perm]
    where flipIndex i@(Index r v) = i { indexValence = flipValence v }
          flipValence Up = Down
          flipValence Down = Up

-- inserts an element at position idx counted from 0
insertAt :: Int -> a -> [a] -> [a]
insertAt idx x xs = lh ++ x:rh
    where (lh, rh) = splitAt idx xs


-- TODO: generate linearly dependent but not identical vector spaces
arbitrarySum :: Gen Calc
arbitrarySum = do
    tensor <- arbitraryTensor
    return $ Op (:+) [tensor, tensor]

arbitraryProduct :: Gen Calc
arbitraryProduct = do
    tensor1 <- oneof [arbitraryTensor, arbitrarySelfContraction]
    tensor2 <- oneof [arbitraryTensor, arbitrarySelfContraction]
    return $ Op (:*) [tensor1, tensor2]

-- only generates identity permutations
arbitraryTensor :: Gen Calc
arbitraryTensor = do
    indices <- listOf arbitrary
    name <- arbitraryIdentifier
    let perm = identity $ length indices
    return $ Tensor name indices perm

-- only generates parsable identifiers
arbitraryIdentifier :: Gen String
arbitraryIdentifier = vectorOf 2 (elements ['a'..'z']) >>= \(x:xs) -> return $ toUpper x : xs
--arbitraryIdentifier = listOf1 (elements ['a'..'z']) >>= \xs -> sized (\n -> return $ take n xs)
