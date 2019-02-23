import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Control.Monad
import Core
import Math.Combinat.Permutations
import Data.Char
import Data.Maybe
import qualified Data.List as L
import qualified Frontend.AbsTensor as Abs
import RenderCalc
import qualified Util as U

-- TODO generate nested calcs with sized
instance Arbitrary Calc where
    arbitrary = arbitraryCalc

instance Arbitrary ValenceType where
    arbitrary = oneof $ map return [Up, Down]

instance Arbitrary ReprType where
    arbitrary = do
        n <- arbitrary :: Gen (Positive Int)
        group <- arbitrary :: Gen GroupType
        return $ ReprType (getPositive n) group

instance Arbitrary GroupType where
    arbitrary = do
        indices <- listOf (arbitrary :: Gen (Positive Int))
        name <- arbitraryIdentifier
        return $ GroupType name (map getPositive indices)

instance Arbitrary Index where
    arbitrary = Index <$> arbitrary <*> arbitrary

arbitraryCalc :: Gen Calc
arbitraryCalc = oneof [
    Number <$> arbitrary,
    arbitraryTensor,
    arbitrarySum,
    arbitraryProduct
 ]

arbitraryContract :: Calc -> Gen Calc
arbitraryContract c = do
    let n = length $ indexFromCalc c
    indices <- shuffle [0..(n-1)]
    let [i1, i2] = sort $ take 2 indices
    return $ Contract i1 i2 c

arbitraryPermute :: Calc -> Gen Calc
arbitraryPermute c = do
    let n = length $ indexFromCalc c
    perm <- shuffle [1..n]
    return $ Permute (toPermutation perm) c

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
                then (U.insertAt slot1 cid . insertAt slot2 (flipIndex cid)) ids
                else (U.insertAt slot2 cid . insertAt slot1 (flipIndex cid)) ids
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

newtype ContractableCalc = ContractableCalc Calc deriving Show
instance Arbitrary ContractableCalc where
    arbitrary = ContractableCalc <$> (suchThat arbitraryCalc (\c -> (length $ indexFromCalc c) > 2))

prop_contractFreeIndices (ContractableCalc calc) = do
    contracted <- arbitraryContract calc
    let res = length (indexFromCalc contracted) == (length (indexFromCalc calc)) - 2
    return $ res

prop_commuteContractPermute (ContractableCalc calc) = do
    permuted <- arbitraryPermute calc
    contracted <- arbitraryContract permuted
    let r1 = renderConsole contracted
    let r2 = renderConsole $ commuteContractPermute' contracted
    return $ counterexample (r1 ++ " /= " ++ r2) (r1 == r2)

-- prop_eliminateMetric ()

prop_renderCalc :: Calc -> Bool
prop_renderCalc calc = (length (renderConsole calc) > 0)

instance Arbitrary Abs.Expr where
    arbitrary = genericArbitrary

instance Arbitrary Abs.Label where
    arbitrary = genericArbitrary

instance Arbitrary Abs.Index where
    arbitrary = genericArbitrary