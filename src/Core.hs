{-# LANGUAGE TupleSections #-}
module Core where

-----------------------------------------------------------------------
-- Book analysis
-----------------------------------------------------------------------

import qualified Frontend.AbsTensor as Abs (
    Expr(..),
    Index(..),
    Label(..)
 )

import System.IO.Unsafe

import Control.Monad.Except
import Test.QuickCheck

import Data.List
import Data.Maybe

import Math.Combinat.Permutations
import Data.Generics.Uniplate.Direct
import Util

import qualified Data.Map as M

import Debug.Trace

type IndexType = ReprType

data GroupType = GroupType {
    groupName :: String,
    groupDims :: [Int]
} deriving (Show, Eq, Ord)

data TensorType = TensorType {
    tensorName :: String,
    tensorIndices :: [IndexType],
    tensorSymmetries :: [SymmetryType]
} deriving (Show, Eq, Ord)


-- make this data type an sum?
data SymmetryType = SymmetryType {
    -- invariant: always length of tensorIndices
    indexSymmetry :: Permutation,
    -- invariant always length 2
    -- either [0,1] [1,0]
    signSymmetry :: Permutation
} deriving (Show, Eq, Ord)

data OpType = OpType {
    opName :: String,
    opIndices :: [IndexType]
} deriving Show

data ReprType = ReprType {
    reprDim :: Int,
    reprGroup :: GroupType
    --reprMetric :: Maybe TensorType
} deriving (Show, Eq, Ord)

data FunctionType = FunctionType {
    funcName :: String,
    funcArity :: Int
} deriving (Show, Eq)

data BookState = BookState {
    bookTensors :: M.Map TensorName TensorType,
    bookOps :: [OpType],
    bookFuncs :: [FunctionType],
    bookCalcs :: M.Map String Calc,
    bookMetrics :: M.Map ReprType TensorType
} deriving Show

nextAnonymousId :: BookState -> Int
nextAnonymousId = length . filter (isPrefixOf "$") . M.keys . bookCalcs

nextAnonymous :: BookState -> String
nextAnonymous = ('$':) . show . nextAnonymousId

currentAnonymous :: BookState -> String
currentAnonymous = ('$':) . show . flip (-) 1 . nextAnonymousId

emptyBook :: BookState
emptyBook = BookState (M.fromList []) [] [] (M.fromList []) (M.fromList [])

lookupGeneratingSet :: TensorType -> [Permutation]
lookupGeneratingSet t = map (uncurry concatPermutations) (zip sg ig)
    where sg = map signSymmetry (tensorSymmetries t)
          ig = map indexSymmetry (tensorSymmetries t)

-- T.a.b [1 0]+
-- S.a.b.c [0 2 1]-

-- T.a.b S.c.d.e
-- [1 0 2 3 4]+ (gs for T in product)
-- [0 1 2 4 3]- (gs for S in product)

-- foo :: TensorType -> TensorType -> [Permutation]
-- foo t1 t2 = zipWith concatPermutations gs1 (repeat $ identity n2) ++
--             zipWith concatPermutations (repeat $ identity n1) gs2
--     where gs1 = lookupGeneratingSet t1
--           gs2 = lookupGeneratingSet t2
--           n1 = length $ tensorIndices t1
--           n2 = length $ tensorIndices t2


type Loff = ([Permutation], [Permutation])

-- inserts a tensor to a generating set
amendGeneratingSet :: TensorType -> Loff -> Loff
amendGeneratingSet t ([], []) = ( map signSymmetry (tensorSymmetries t)
                                , map indexSymmetry (tensorSymmetries t))
amendGeneratingSet t (sg, ig) = (sg ++ sg',
    zipWith concatPermutations (repeat $ identity n) ig ++
    zipWith concatPermutations ig' (repeat $ identity n'))
    where n = length $ tensorIndices t
          n' = permutationSize g
          (g:_) = ig
          sg' = map signSymmetry (tensorSymmetries t)
          ig' = map indexSymmetry (tensorSymmetries t)

-- (zipWith concatPermutations (repeat $ identity n) gs) ++
                        --    sg', zipWith concatPermutations gs' (repeat $ identity n'))

termGeneratingSet :: [TensorType] -> [Permutation]
termGeneratingSet ts = (uncurry . zipWith) concatPermutations
    $ foldr amendGeneratingSet ([], []) (traceShowId ts)

lookupMetric' :: e -> ReprType -> BookState -> Either e TensorType
lookupMetric' e r = (maybeToEither e) . lookupMetric r

lookupMetric :: ReprType -> BookState -> Maybe TensorType
lookupMetric r bs = M.lookup r (bookMetrics bs)

lookupTensor' :: e -> String -> BookState -> Either e TensorType
lookupTensor' e s = (maybeToEither e) . lookupTensor s

lookupTensor :: String -> BookState -> Maybe TensorType
lookupTensor l bs = M.lookup l (bookTensors bs)

lookupOp :: String -> BookState -> Maybe OpType
lookupOp l bs = listToMaybe $ filter (\t -> opName t == l) $ bookOps bs

lookupOp' :: e -> String -> BookState -> Either e OpType
lookupOp' e s = maybeToEither e . lookupOp s

lookupCalc' :: e -> String -> BookState -> Either e Calc
lookupCalc' e s = maybeToEither e . lookupCalc s

lookupCalc :: String -> BookState -> Maybe Calc
lookupCalc s bs = M.lookup s (bookCalcs bs)

-----------------------------------------------------------------------
-- Transformation
-----------------------------------------------------------------------

type TensorName = String
type OpName = String

-- tensor T{ a, b, c, d, e, f(4): SO(3,1) }
--
-- \x y -> ((* c(x,y) // T:a b x d e f) (T a b c y e f))
--                         2 3 0 4 5 6     2 3 7 1 5 6  --
--                         2 2 0 2 2 2     2 2 2 1 2 2  --
--                         * (2 3 4 5 6) (2 3 7 5 6) (c)
--                         -- T T + T T
-- T.a.b.c T^a.e.f + T.a.b.c T.e^a.f
-- T.b S.a + T.a S.b - Permute [2, 1] T S

-- T.a.b.c.d

-- \a b c d -> a.b.c + d

data Calc
    = Number Rational
    | Tensor TensorName [Index]
    | Prod Calc Calc
    | Sum Calc Calc
    | Transform String [Calc]
    | Power Int Calc
    | Permute Permutation Calc
    | Contract Int Int Calc
    | Op OpName [Index] Calc
--    | Lambda Calc
--    | App Calc Calc
--    | Var Int
    deriving (Show, Eq, Ord)

instance Uniplate Calc where
    uniplate (Sum s1 s2) = plate Sum |* s1 |* s2
    uniplate (Prod f1 f2) = plate Prod |* f1 |* f2
    uniplate (Transform s c) = plate Transform |- s ||* c
    uniplate (Power i c) = plate Power |- i |* c
    uniplate (Permute p c) = plate Permute |- p |* c
    uniplate (Contract i1 i2 c) = plate Contract |- i1 |- i2 |* c
    uniplate (Number n) = plate Number |- n
    uniplate (Tensor n idx) = plate Tensor |- n |- idx
    uniplate (Op n idx c) = plate Op |- n |- idx |* c

data Index = Index {
    indexRepr :: IndexType,
    indexValence :: ValenceType
} deriving (Show, Eq, Ord)

data ValenceType = Up | Down deriving (Show, Eq, Ord)

infixl 5 |*|
infixl 4 |+|

(|*|) :: Calc -> Calc -> Calc
c1 |*| c2 = Prod c1 c2

(|+|) :: Calc -> Calc -> Calc
c1 |+| c2 = Sum c1 c2

setValence :: Calc -> Int -> ValenceType -> Calc
setValence (Tensor n idx) i v =
    let (lh, index:rh) = splitAt i idx
        newIndex = Index (indexRepr index) v
        newIndices = lh ++ (newIndex:rh)
    in Tensor n newIndices
setValence (Op n idx c) i v
  | i < (length idx) =
    let (lh, index:rh) = splitAt i idx
        newIndex = Index (indexRepr index) v
        newIndices = lh ++ (newIndex:rh)
    in Op n newIndices c
setValence (Op n idx c) i v
  | i >= (length idx) = Op n idx (setValence c (i - (length idx)) v)
setValence _ _ _ = undefined

changeValence :: BookState -> Calc -> Int -> Either String Calc
changeValence bs c i = do
    --let indices = traceShow ((show i) ++ ":") $ traceShowId $ freeIndexFromCalc c
    let indices = freeIndexFromCalc c
    let index = indices!!i
    let r = indexRepr index
    let valence = indexValence index
    let targetValence = otherValence valence
    metric <- lookupMetric' ("No metric for representation " ++ (show r)) r bs
    let newIndex = Index r targetValence
    let newCalc = (Tensor (tensorName metric) [newIndex, newIndex]) |*| c
    let contracted = Contract 1 (i+2) newCalc
    let cycle = cycleLeft (i + 1)
    let rest = identity $ (length indices) - (i + 1)
    let perm = concatPermutations cycle rest
    return $ Permute perm contracted

switchValence :: BookState -> Calc -> Int -> Either String Calc
switchValence bs c i = do
    let indices = freeIndexFromCalc c
    let index = indices!!i
    let r = indexRepr index
    let valence = indexValence index
    let targetValence = otherValence valence
    metric <- lookupMetric' ("No metric for representation " ++ (show r)) r bs
    let newIndex = Index r valence
    let newCalc = (Tensor (tensorName metric) [newIndex, newIndex]) |*| (setValence c i targetValence)
    let contracted = Contract 1 (i+2) newCalc
    let cycle = cycleLeft (i + 1)
    let rest = identity $ (length indices) - (i + 1)
    let perm = concatPermutations cycle rest
    return $ Permute perm contracted

otherValence :: ValenceType -> ValenceType
otherValence Up = Down
otherValence Down = Up




-- Free indices in calc
freeIndexFromCalc :: Calc -> [Index]
freeIndexFromCalc x = case x of
  (Number _) -> []
  (Power _ _) -> []
  (Tensor _ []) -> []
  (Tensor _ idx) -> idx
  (Sum first _) -> freeIndexFromCalc first
  (Prod f1 f2) -> freeIndexFromCalc f1 ++ freeIndexFromCalc f2
  -- Permuted indices must be of same type so can just pass through
  (Permute p c) -> permuteList (inverse p) $ freeIndexFromCalc c
  (Contract i1 i2 c)
      | i1 < i2 -> deleteAt i1 $ deleteAt i2 (freeIndexFromCalc c)
      | i1 > i2 -> deleteAt i2 $ deleteAt i1 (freeIndexFromCalc c)
  (Op n idx c) -> idx ++ freeIndexFromCalc c

nFreeIndices :: Calc -> Int
nFreeIndices = length . freeIndexFromCalc

tensorTypeFromCalc :: String -> Calc -> TensorType
tensorTypeFromCalc l c = TensorType l (map indexRepr $ freeIndexFromCalc c) undefined

-- indexTypeFromCalc :: Calc -> [IndexType]
-- indexTypeFromCalc (Number _) = []
-- indexTypeFromCalc (Tensor _ []) = []
-- indexTypeFromCalc (Tensor _ idx) = map indexToIndexType idx
-- indexTypeFromCalc (Sum first _) = indexTypeFromCalc first
-- indexTypeFromCalc (Prod f1 f2) = indexTypeFromCalc f1 ++ indexTypeFromCalc f2
-- -- Permuted indices must be of same type so can just pass through
-- indexTypeFromCalc (Permute p c) = indexTypeFromCalc c
-- indexTypeFromCalc (Contract i1 i2 c)
--     | i1 < i2 = deleteAt i1 $ deleteAt i2 (indexTypeFromCalc c)
-- indexTypeFromCalc (Op n idx c) = (map indexToIndexType idx) ++ indexTypeFromCalc c

indexToIndexType :: Index -> IndexType
indexToIndexType i = indexRepr i
--indexToIndexType i = ReprType (reprDim $ indexRepr i) (reprGroup $ indexRepr i)

type Error = Except String

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e (Nothing) = Left e
maybeToEither _ (Just x)  = Right x

-- T.a.b^b * T.c.d.e = C 1 2 (T.a.x.y T.c.d.e)
-- [1,2] dummies
-- [0,3,4,5] frees
-- [a,c,d,e]


-- C 1 2 $ C 1 2 $ C 1 2 T [..10]
-- [1,2] (5,6) ++ [1,2] (3,4) ++ [1,2]
-- ([5,6,3,4,1,2], [1,2,3,4])
--  [5,6,3,4,1,2], [7,8,9,10]

-- C 0 1 P 2,0,1 T 0 1 2
-- C 0 1 P' 0,2,1 T 0 1 2
-- i1' = permute i1 (inverse inverse p * inverse p')
-- 0 = permute (inverse (2,0,1)) N

-- [5,6,3,4,1,2]


isMonoTerm :: Calc -> Bool
isMonoTerm c = case c of
  (Number _) -> True
  (Power _ _) -> True
  (Tensor _ _) -> True
  (Sum _ _) -> False
  (Prod f1 f2) -> isMonoTerm f1 && isMonoTerm f2
  (Permute p c) -> isMonoTerm c
  (Contract i1 i2 c) -> isMonoTerm c
  (Op n idx c) -> isMonoTerm c

-- prop_allIndexSlotsPreservesLength :: Calc -> Property
-- prop_allIndexSlotsPreservesLength c =
--     isMonoTerm c ==> length frees + length dummies == nIndices
--     where (frees, dummies) = allIndexSlots c
--           nIndices = length $ allIndices c

allIndices :: Calc -> [Index]
allIndices c | isMonoTerm c = case c of
  (Number _) -> []
  (Power _ _) -> []
  (Tensor _ []) -> []
  (Tensor _ idx) -> idx
  (Prod f1 f2) -> allIndices f1 ++ allIndices f2
  (Permute p c) -> permuteList (inverse p) $ allIndices c
  (Contract i1 i2 c)
      | i1 < i2 -> deleteAt i1 $ deleteAt i2 (allIndices c)
      | i1 > i2 -> deleteAt i2 $ deleteAt i1 (allIndices c)
  (Op n idx c) -> idx ++ allIndices c
allIndices c = undefined


offsetSequence :: Int -> [Int] -> [Int]
offsetSequence n = map (n +)

offsetTupleSequence :: Int -> [(Int, Int)] -> [(Int, Int)]
offsetTupleSequence n vinst = zip (offsetSequence n l1) (offsetSequence n l2)
    where (l1, l2) = unzip vinst

-- (True, _) => dummy index
-- (False, _) => free index
type IndexTagType = (Bool, Int)

-- instance Monad IndexTagType where
--     (>>=) a b = undefined
--     return a = a

-- type Dummy = (Int, Int)


-- C 0 1 $ C 0 1 $ P [0 1 2 3 4 5]
-- C 0 1 $ ([2 3], [(0, 1)])
-- ([4 5], [(0, 1), (2, 3)])

-- Contract relative to current frees
type RelDummy = (Int, Int)

-- Contract relative to absolut mono-term indices
type AbsDummy = (Int, Int)

-- returns a list of frees and dummies
absoluteDummies :: [RelDummy] -> Int -> [AbsDummy]
absoluteDummies rds n = snd $ collectFreesAndDummies rds n

collectFreesAndDummies :: [RelDummy] -> Int -> ([Int], [AbsDummy])
collectFreesAndDummies rds n = foldr adjustDummy (inIndices, []) (traceShow ("rds", rds) (reverse rds))
    where adjustDummy (i1, i2) (indices, absDummies)
            | i1 < i2 = (deleteAt i1 $ deleteAt i2 indices, (indices !! i1, indices !! i2) : absDummies)
            | i1 > i2 = (deleteAt i2 $ deleteAt i1 indices, (indices !! i1, indices !! i2) : absDummies)
          inIndices = [0..n - 1]

-- Given frees and dummies want to generate a sorting permutation for that
-- gives the indices in the two groups as [frees dummies]
-- Example:
--  T.a^a.b.c.d^c
--  Frees: [2 4]  Dummies: [(0,1), (3,5)]
--  Put free indices and dummy pairs together as in
--  L1 = 2 4 0 1 3 5
-- then the sortinPermutationAsc gives P such that P L1 = [0..5] (http://hackage.haskell.org/package/combinat-0.2.9.0/docs/Math-Combinat-Permutations.html#g:8)
-- which means that P' = P^-1 satisfies P' [0..5] = L1
-- i.e. P' sorts the indices into [frees dummies]

-- | Returns permutation that sorts dummies to the end
--                                                           _   _
--                                                          | | | |
-- >>> permuteList (sortDummyPermutation [(0,1), (0,1)] 6) [1,2,3,4,5,6]
-- [5,6,1,2,3,4]
--       
--                                                           _   ___
--                                                          | | |   |
-- >>> permuteList (sortDummyPermutation [(0,2), (0,1)] 6) [1,2,3,4,5,6]
-- [4,6,1,2,3,5]
sortDummyPermutation :: [RelDummy] -> Int -> Permutation
sortDummyPermutation rds n = inverseSorting 
    where (frees, dummyPairs) = collectFreesAndDummies rds n
          tupleToList (i1, i2) = [i1, i2]
          freesDummies = frees ++ concatMap tupleToList (reverse dummyPairs)
          inverseSorting = sortingPermutationAsc freesDummies

-- | Dummy generating set under permutation
-- >>> let [dg] = dummyGS [(0,1),(0,1)] (identity 6)
-- >>> fromPermutation dg
-- [3,4,1,2,5,6]
--
-- (Remember permutation acts on the right so p * q acts as ([..] p) q)
-- >>> let [dg] = dummyGS [(0,1),(0,1)] (cycleLeft 6)
-- >>> let p1 = dg `multiply` cycleLeft 6
-- >>> let p2 = cycleLeft 6 `multiply` (toPermutation [3,4,1,2,5,6])
-- >>> p1 == p2
-- True
dummyGS :: [RelDummy] -> Permutation -> [Permutation]
dummyGS [] _ = []
dummyGS rds p = map (commute p) dummySwaps
    where n = permutationSize p
          (frees, dummies) = collectFreesAndDummies rds n
          swapPerm (i1, i2) (j1, j2) = multiply t1 t2
            where t1 = (transposition n (i1 + 1, j1 + 1))
                  t2 = (transposition n (i2 + 1, j2 + 1))
          dummySwaps = zipWith swapPerm (init dummies) (tail dummies)

dummyGSWithSign :: [RelDummy] -> Permutation -> [Permutation]
dummyGSWithSign rds p = map addSign dgs
    where dgs = dummyGS rds p
          addSign = concatPermutations (identity 2)

-- | Commute permutation
-- [..] x' P = [..] P x ==> x' = P * x * P^-1
--
-- (Remember permutation acts on the right so p * q acts as ([..] p) q)
-- >>> let p = cycleLeft 5
-- >>> let x = transposition 5 (1,2)
-- >>> (commute p x) `multiply` p == p `multiply` x
-- True
commute :: Permutation -> Permutation -> Permutation
commute p x =  p `multiply` x `multiply` (inverse p)

-- -- returns a list of frees and dummies
-- collectDummies :: [RelDummy] -> Calc -> ([Int], [AbsDummy])
-- collectDummies c = case c of
--   (Number _) -> ([], [])
--   (Power _ _) -> ([], [])
--   (Tensor _ []) -> ([], [])
--   (Tensor _ idx) -> ([0..length idx - 1], [])
--   (Prod f1 f2) -> (frees1 ++ offsetSequence n1 frees2, dummies1 ++ offsetTupleSequence n1 dummies2)
--     where (frees1, dummies1) = collectDummies f1
--           (frees2, dummies2) = collectDummies f2
--           n1 = length frees1 + 2 * length dummies1
--   (Contract i1 i2 c)
--       | i1 < i2 -> (deleteAt i1 $ deleteAt i2 frees, (frees !! i1, frees !! i2) : dummies)
--       | i1 > i2 -> (deleteAt i2 $ deleteAt i1 frees, (frees !! i1, frees !! i2) : dummies)
--     where (frees, dummies) = collectDummies c
--   _ -> undefined


-- allIndexTags :: Calc -> [IndexTagType]
-- allIndexTags c = case c of
--   (Number _) -> []
--   (Power _ _) -> []
--   (Tensor _ []) -> []
--   (Tensor _ idx) -> map (True,) [0..length idx]
--   (Prod f1 f2) -> tags1 ++ map (\x -> (fst x, snd x + length tags1)) tags2
--     where tags1 = allIndexTags f1
--           tags2 = allIndexTags f2
--   (Permute p c) -> permuteList (inverse p) $ allIndexTags c
--   (Contract i1 i2 c) -> toDummy i1 $ toDummy i2 (allIndexTags c)
--       | i1 < i2 -> deleteAt i1 $ deleteAt i2 (freeIndexFromCalc c)
--       | i1 > i2 -> deleteAt i2 $ deleteAt i1 (freeIndexFromCalc c)
--     where toDummy i = transformAt i ((True,) . snd)
--   (Op n idx c) -> undefined
--   _ -> undefined

-- allIndexSlots :: Calc -> ([Int], [Int])
-- allIndexSlots c = (map snd frees, map snd dummies)
--     where (dummies, frees) = partition fst $ allIndexTags c 

-- allIndexSlots :: Calc -> ([Int], [Int])
-- allIndexSlots c = case c of
--   (Number _) -> ([], [])
--   (Power _ _) -> ([], [])
--   (Tensor _ []) -> ([], [])
--   (Tensor _ idx) -> ([0..length idx], [])
--   (Prod f1 f2) -> (frees', dummies')
--     where (free1, dummies1) = allIndexSlots f1
--           (free2, dummies2) = allIndexSlots f2
--           nIndices1 = length free1 + length dummies1
--           frees' = free1 ++ offsetSequence nIndices1 free2
--           dummies' = dummies1 ++ offsetSequence nIndices1 dummies2
--   (Permute p c) -> permuteList (inverse p) $ allIndexSlots c
--   (Contract i1 i2 c)
--       | i1 < i2 -> deleteAt i1 $ deleteAt i2 (freeIndexFromCalc c)
--       | i1 > i2 -> deleteAt i2 $ deleteAt i1 (freeIndexFromCalc c)
--   (Op n idx c) -> idx ++ freeIndexFromCalc c

