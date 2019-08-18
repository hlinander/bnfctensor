{-# LANGUAGE TemplateHaskell #-}

module Test where

import Debug.Trace
import System.IO.Unsafe
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Math.Combinat.Permutations
import Data.Char
import qualified Data.Map as M
import qualified Data.List as L

import Core
import Transform
import qualified Frontend.AbsTensor as Abs
import RenderCalc
import qualified Util as U
import RustPerm

instance Arbitrary Calc where
    shrink = shrinkCalc
    arbitrary = sized foo
        where foo 0 = oneof [ Number <$> arbitrary, arbitraryTensor, arbitrarySelfContraction ]
              foo n = frequency [
                (2, foo (n-1)),
                (1, Number <$> arbitrary),
                (2, arbitraryTensor),
                (2, resize (n-1) arbitrarySum),
                (2, resize (n-1) arbitraryProduct),
                (1, resize (n-1) arbitraryContraction),
                (2, arbitrarySelfContraction),
                (2, arbitraryMetric)
               ]

shrinkCalc :: Calc -> [Calc]
shrinkCalc c = case c of
    Prod f1 f2 -> [ Tensor "shrinked" (indexFromCalc f1 ++ indexFromCalc f2) ]
        ++ [ Prod s1 s2 | (s1,s2) <- shrink (f1,f2) ]
    Sum t1 t2 -> [t1,t2]
    Op n idx calc -> [Op n idx s | s <- shrink calc]
    _ -> []

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

isContractible :: Calc -> Bool
isContractible c = case map length indexGroups of
        [] -> False
        xs -> minimum xs >= 2
    where indices = indexFromCalc c
          indexGroups = map snd $ M.toList $ M.fromListWith (++) [(indexRepr i, [i]) | i <- indices]


prop_contract :: Int -> Int -> Int -> Property
prop_contract i1 i2 i = i1 >= 0    && i2 >= 0
    && i >= 0
    ==> 1 == 1

prop_replaceIndex :: Calc -> Int -> Index -> Property
prop_replaceIndex c i idx = length (indexFromCalc c) > i && i >= 0
        ==> counterexample (renderConsole c ++ " -> " ++ renderConsole replaced) $ (indexFromCalc replaced) !! i == idx
        where replaced = replaceIndex c i idx

arbitraryContraction = suchThat arbitrary (\c -> length (indexFromCalc c) >= 2) >>= arbitraryContract

arbitraryContract :: Calc -> Gen Calc
arbitraryContract c = do
    let indices = indexFromCalc c
    let n = length indices
    i1 <- choose (0, n-1)
    i2 <- suchThat (choose (0, n-1)) (/= i1)
    let roff = replaceIndex c i2 (flipIndex (indices !! i1))
    return $ Contract i1 i2 roff

flipIndex i@(Index r v) = i { indexValence = f v }
  where f Up = Down
        f Down = Up

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
            cid <- arbitrary
            slot1 <- choose (0, length ids - 1)
            slot2 <- suchThat (choose (0, length ids - 1)) (not . (==) slot1)
            let cids = if slot1 > slot2
                then (U.insertAt slot1 cid . U.insertAt slot2 (flipIndex cid)) ids
                else (U.insertAt slot2 cid . U.insertAt slot1 (flipIndex cid)) ids
            let [s1, s2] = L.sort [slot1, slot2]
            return $ (Contract s1 s2) $ Tensor n cids
                where flipIndex i@(Index r v) = i { indexValence = flipValence v }
                      flipValence Up = Down
                      flipValence Down = Up
        _ -> undefined

-- TODO: generate linearly dependent but not identical vector spaces
arbitrarySum :: Gen Calc
arbitrarySum = do
    calc <- arbitrary 
    return $ calc |+| calc

arbitraryProduct :: Gen Calc
arbitraryProduct = do
    tensor1 <- arbitrary
    tensor2 <- arbitrary
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


arbitraryMetric :: Gen Calc
arbitraryMetric = do
    index1 <- arbitrary
    index2 <- arbitrary
    return $ Tensor "g" [index1, index2]


prop_indexFromCalcUnderContraction :: Calc -> Property
prop_indexFromCalcUnderContraction c = length (indexFromCalc c) > 2 ==>
    forAll (arbitraryContract c) invariant
        where invariant c' = length (indexFromCalc c') == (length (indexFromCalc c)) - 2

-- NEEDS HELP
-- prop_commuteContractPermute :: Calc -> Property
-- prop_commuteContractPermute calc = length (indexFromCalc calc) > 2
--     ==> do
--     permuted <- arbitraryPermute calc
--     contracted <- arbitraryContract permuted
--     let r1 = renderConsole contracted
--     let r2 = renderConsole $ commuteContractPermute' contracted
--     return $ counterexample (r1 ++ " /= " ++ r2) (r1 == r2)

prop_renderCalc :: Calc -> Bool
prop_renderCalc calc = not (null (renderConsole calc))

prop_eliminateMetricsIsValid :: Calc -> Property
prop_eliminateMetricsIsValid c = (length (indexFromCalc c) > 2)
    ==> forAll (arbitraryContract c) $ \calc -> do
        let condition = validCalc calc ==> validCalc (eliminateMetrics calc)
        counterexample (renderConsole calc) condition

transformIsValid :: (Calc -> Calc) -> Calc -> Property
transformIsValid f calc = do 
    let condition = validCalc calc ==> validCalc (f calc)
    counterexample (renderConsole calc) condition

prop_simplifyIsValid :: Calc -> Property
prop_simplifyIsValid = transformIsValid simplify

prop_sortCalcIsValid :: Calc -> Property
prop_sortCalcIsValid = transformIsValid sortCalc

prop_simplifyContractIsValid :: Calc -> Property
prop_simplifyContractIsValid = transformIsValid simplifyContract

prop_commuteContractPermuteIsValid :: Calc -> Property
prop_commuteContractPermuteIsValid = transformIsValid commuteContractPermute

prop_simplifyFactorsIsValid :: Calc -> Property
prop_simplifyFactorsIsValid = transformIsValid simplifyFactors

prop_simplifyTermsIsValid :: Calc -> Property
prop_simplifyTermsIsValid = transformIsValid simplifyTerms

prop_simplifyPermutationsIsValid :: Calc -> Property
prop_simplifyPermutationsIsValid = transformIsValid sortCalc

instance Arbitrary Abs.LabelList where
    arbitrary = genericArbitrary

instance Arbitrary Abs.Expr where
    arbitrary = genericArbitrary

instance Arbitrary Abs.Label where
    arbitrary = genericArbitrary

instance Arbitrary Abs.Index where
    arbitrary = genericArbitrary

prop_insertAt :: (Eq a) => Int -> a -> [a] -> Property
prop_insertAt idx x list = idx >=0 && idx <= length list
    ==> (U.insertAt idx x list) !! idx == x

prop_image :: Int -> Property
prop_image n = n >= 0 ==> forAll (shuffle [1..(n+1)*2]) assertion
    where assertion l = (permuteList (perm l) testList) !! (U.image (perm l) n) == testList !! n
          perm = toPermutation
          testList = [0..]

return []
runTests = $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 500 })
