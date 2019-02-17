{-# LANGUAGE DeriveDataTypeable #-}
module Core where

-----------------------------------------------------------------------
-- Book analysis
-----------------------------------------------------------------------

import qualified Frontend.AbsTensor as Abs (
    Expr(..),
    Index(..),
    Label(..)
 )

import Control.Monad.Reader
import Control.Monad.Except

import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.Ratio

import Data.Generics.Uniplate.Direct
import Math.Combinat.Permutations
import Util

import qualified Data.Map as M
import qualified Tensor as T

-- import Control.Lens.Plated
-- import Data.Data.Lens
-- import Data.Data

import Debug.Trace

type IndexType = ReprType

data GroupType = GroupType {
    groupName :: String,
    groupDims :: [Int]
} deriving (Eq, Ord)

instance Show GroupType where
    showsPrec i g = showString (groupName g) . showList (groupDims g)

createRepr :: GroupType -> Int -> ReprType
createRepr = flip ReprType

data TensorType = TensorType {
    tensorName :: String,
    tensorIndices :: [IndexType]
} deriving Eq

instance Show TensorType where
    showsPrec i t = showString (tensorName t)
        . showString " { "
        . showList (tensorIndices t)
        . showString " }"

data OpType = OpType {
    opName :: String,
    opIndices :: [IndexType]
}

instance Show OpType where
    showsPrec i t = showString (opName t)
        . showString " { "
        . showList (opIndices t)
        . showString " }"

data ReprType = ReprType {
    reprDim :: Int,
    reprGroup :: GroupType
--    reprMetric :: Maybe TensorType
} deriving (Eq, Ord)

instance Show ReprType where
    showsPrec i repr =
        (showParen True
        $ shows (reprDim repr))
        . showString ": "
        . shows (reprGroup repr)

-- class Metric a where
--     getMetricType :: a -> TensorType
--     getMetric :: a -> ValenceType -> ValenceType -> Calc
--
-- instance Metric ReprType where
--     getMetricType repr = TensorType "g" [repr, repr]
--     getMetric repr (v1, v2) = Tensor "g" [Index repr v1, Index repr v2]

data FunctionType = FunctionType {
    funcName :: String,
    funcArity :: Int
} deriving (Show, Eq)

data BookState = BookState {
    -- bookTensors :: M.Map TensorName TensorType,
    bookTensors :: [TensorType],
    bookOps :: [OpType],
    bookFuncs :: [FunctionType],
    bookCalcs :: M.Map String Calc
    -- bookVariables :: [(String,Int)] let foo = a+b+c+d
} deriving Show


-- Representation should have maybe metric associated
-- standardTensors = [TensorType {
--     tensorName = "g",
--     tensorIndices = [IndexType 1 (GroupType "metric" [1]) "a"]
-- }]

emptyBook :: BookState
emptyBook = BookState [] [] [] $ M.fromList []

lookupTensor' :: e -> String -> BookState -> Either e TensorType
lookupTensor' e s = (maybeToEither e) . lookupTensor s

lookupTensor :: String -> BookState -> Maybe TensorType
lookupTensor l bs = listToMaybe $ filter (\t -> tensorName t == l) $ bookTensors bs

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

data Calc
    = Number Rational
    | Sum Calc Calc
    | Prod Calc Calc
    | Transform String [Calc]
    | Power Int Calc
    | Permute Permutation Calc
    | Contract Int Int Calc
    | Tensor TensorName [Index]
    | Op OpName [Index] Calc
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
    indexRepr :: ReprType,
    indexValence :: ValenceType
} deriving (Show, Eq, Ord)

data ValenceType = Up | Down deriving (Show, Eq, Ord)

infixl 5 |*|
infixl 4 |+|

(|*|) :: Calc -> Calc -> Calc
c1 |*| c2 = Prod c1 c2

(|+|) :: Calc -> Calc -> Calc
c1 |+| c2 = Sum c1 c2

tensorTypeFromCalc :: String -> Calc -> TensorType
tensorTypeFromCalc l c = TensorType l (indexTypeFromCalc c)

indexTypeFromCalc :: Calc -> [IndexType]
indexTypeFromCalc (Number _) = []
indexTypeFromCalc (Tensor _ []) = []
indexTypeFromCalc (Tensor _ idx) = map indexToIndexType idx
indexTypeFromCalc (Sum first _) = indexTypeFromCalc first
indexTypeFromCalc (Prod f1 f2) = indexTypeFromCalc f1 ++ indexTypeFromCalc f2
-- Permuted indices must be of same type so can just pass through
indexTypeFromCalc (Permute p c) = indexTypeFromCalc c
indexTypeFromCalc (Contract i1 i2 c)
    | i1 < i2 = deleteAt i1 $ deleteAt i2 (indexTypeFromCalc c)
indexTypeFromCalc (Op n idx c) = (map indexToIndexType idx) ++ indexTypeFromCalc c

indexToIndexType :: Index -> IndexType
indexToIndexType i = ReprType (reprDim $ indexRepr i) (reprGroup $ indexRepr i)

type Error = Except String

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e (Nothing) = Left e
maybeToEither _ (Just x)  = Right x

calcFromExpr :: Abs.Expr -> BookState -> Either String Calc
calcFromExpr x bs = second fst $ runExcept $ runReaderT (calcFromExpr' x) bs

calcFromExpr' :: Abs.Expr -> (ReaderT BookState (Except String)) (Calc, [(String, Index)])
calcFromExpr' x = case x of
  Abs.Add expr1 expr2 -> do
    (calc1, idx1) <- calcFromExpr' expr1
    (calc2, _) <- calcFromExpr' expr2
    return (calc1 |+| calc2, idx1)
  Abs.Sub expr1 expr2 -> do
    (calc1, idx1) <- calcFromExpr' expr1
    (calc2, _) <- calcFromExpr' expr2
    return (calc1 |+| Number (-1) |*| calc2, idx1)
  Abs.Neg expr -> do
    (calc, idx) <- calcFromExpr' expr
    return (Number (-1) |*| calc, idx)
  Abs.Div expr (Abs.Number num) -> do
    (calc, idx) <- calcFromExpr' expr
    return (calc |*| Number (1 % num), idx)
  Abs.Div expr (Abs.Fraction p q) -> do
    (calc, idx) <- calcFromExpr' expr
    return (calc |*| Number (p % q), idx)
  Abs.Div _ _ -> undefined
  Abs.Tensor (Abs.Label l) absIdx -> do
    maybeCalc <- asks (lookupCalc l)
    tensorType <- asks (lookupTensor' ("Undefined tensor '" ++ l ++ "'") l) >>= liftEither
    let indexTypes = tensorIndices tensorType
    let freeSlots = T.freeIndexPos x
    let (indices, perm, contractions, sortedIdxAndLabels) = processIndexedObject absIdx indexTypes freeSlots
    let calc = case maybeCalc of
            Just storedCalc -> storedCalc
            Nothing -> Tensor l indices
    let contractedCalc = contractNew contractions calc
    return (Permute perm contractedCalc, sortedIdxAndLabels)
  Abs.Anon i absIdx -> do
    let varName = '$' : show i
    maybeCalc <- asks (lookupCalc' ("Undefined expression identifier '$" ++ show i ++ "'") varName) >>= liftEither
    tensorType <- asks (lookupTensor' ("Undefined expression identifier '$" ++ show i ++ "'") varName) >>= liftEither
    let indexTypes = tensorIndices tensorType
    let freeSlots = T.freeIndexPos x
    let (indices, perm, contractions, sortedIdxAndLabels) = processIndexedObject absIdx indexTypes freeSlots
    --let calc = maybeCalc
    let contractedCalc = contractNew contractions maybeCalc
    return (Permute perm contractedCalc, sortedIdxAndLabels)
    --case maybeCalc of
    --  Nothing -> lift Nothing
    --  Just storedCalc -> do
  Abs.Op (Abs.Label l) absIdx expr -> do
    (calc, idx) <- calcFromExpr' expr
    opType <- asks (lookupOp' ("Undefined operator '" ++ l ++ "'") l) >>= liftEither
    let calcAbsIndices = map labeledIndexToAbsIndex idx
    let indexTypes = opIndices opType ++ indexTypeFromCalc calc
    let allIndices = absIdx ++ calcAbsIndices
    let freeSlots = T.freeIndicesWithPos allIndices []
    let (indices, perm, contractions, sortedIdxAndLabels) = processIndexedObject allIndices indexTypes freeSlots
    let contractedCalc = contractNew contractions (Op l (take (length absIdx) indices) calc)
    let permCalc = Permute perm contractedCalc
    return (permCalc, sortedIdxAndLabels)
  Abs.Number p -> return (Number (fromInteger p), [])
  Abs.Fraction p q -> return (Number (p % q), [])
  Abs.Mul expr1 expr2 -> do
    (calc1, idx1) <- calcFromExpr' expr1
    (calc2, idx2) <- calcFromExpr' expr2
    let offset = length (T.freeIndexSlots expr1)
    let (perm, contracted, f') = processBinaryIndexed idx1 idx2 offset
    let res = if isIdentityPermutation perm
                then (contractNew contracted $ calc1 |*| calc2, f')
                else (Permute perm $ contractNew contracted $ calc1 |*| calc2, f')
    return $ res
  Abs.Func (Abs.Label name) (expr:[]) -> do
    (calc, idx) <- calcFromExpr' expr
    return $ (execute name calc, idx)

  x -> (return $ (traceShow ("vinsten: " ++ show x) (Number 1), []))

labeledIndexToAbsIndex (l, Index{indexValence=Up}) = Abs.Upper (Abs.Label l)
labeledIndexToAbsIndex (l, Index{indexValence=Down}) = Abs.Lower (Abs.Label l)

type LabeledIndex = (String, Index)
type AbsIndexPos = (Abs.Index, Int)
type IndexedData = ([Index], Permutation, [ContractPairNew], [LabeledIndex])

processIndexedObject :: [Abs.Index] -> [IndexType] -> [AbsIndexPos] -> IndexedData
processIndexedObject absIdx indexTypes freeSlots = (indices, perm, contractions, sortedIdxAndLabels)
    where absIndicesWithType = zip absIdx $ indexTypes :: [(Abs.Index, IndexType)]
          indices = map (uncurry indexTypeToIndex) absIndicesWithType :: [Index]
          -- Which of the indices are free?
          freeSlotsPos = map snd freeSlots :: [Int]
          freeSlotsLabels = map (indexLabel.fst) freeSlots :: [String]
          freeIndices = map (indices!!) freeSlotsPos :: [Index]
          freeIndicesAndLabels = zip freeSlotsLabels freeIndices :: [(String, Index)]
          sortedIdxAndLabels = sortBy (\(a, _) (b, _) -> compare a b) freeIndicesAndLabels
          perm = inverse $ sortingPermutationAsc freeSlotsLabels
          contractions = selfContractedIndexPairs (zip absIdx indices)

type BinaryIndexedData = (Permutation, [ContractPairNew], [LabeledIndex])

processBinaryIndexed :: [LabeledIndex] -> [LabeledIndex] -> Int -> BinaryIndexedData
processBinaryIndexed idx1 idx2 offset = (perm, contracted, f')
    where contracted = contractedPairsNew idx1 idx2
          c1 = map (\(_,_,i) -> i) $ map fst contracted
          c2 = map (\(_,_,i) -> i) $ map snd contracted
          c2' = map (\i -> i - offset) c2
          f1 = foldr deleteAt idx1 c1
          f2 = foldr deleteAt idx2 c2'
          f = map fst $ f1 ++ f2
          perm = inverse $ sortingPermutationAsc f
          f' = permuteList (inverse $ perm) (f1 ++ f2)

-- Create nested contractions for a list of contraction pairs of slots
contractNew :: [ContractPairNew] -> Calc -> Calc
contractNew [] expr = expr
contractNew (((_, _, i1), (_, _, i2)):rest) expr = Contract i1 i2 $ contractNew rest expr

type IndexSlotNew = (String, Index, Int)
type ContractPairNew = (IndexSlotNew, IndexSlotNew)

labelEqual :: ContractPairNew -> Bool
labelEqual ((l1, _, _),(l2, _, _)) = l1 == l2

-- labelEqual :: ContractPair -> Bool
-- labelEqual ((l1, _),(l2, _)) = indexLabel l1 == indexLabel l2

contractedPairsNew:: [(String, Index)] -> [(String, Index)] -> [ContractPairNew]
contractedPairsNew free1 free2 = nestedPairs
    where -- Get indices with same label between factors
          free1Pos = map (\((l, index), idx) -> (l, index, idx)) $ zip free1 [0..]
          free2Pos = map (\((l, index), idx) -> (l, index, idx)) $ zip free2 [0..]
          cartProd = [(i1, i2) | i1 <- free1Pos, i2 <- free2Pos]
          intersection = filter (\((l1, _, _), (l2, _, _)) -> l1 == l2) cartProd
          (lh, rh) = (map fst intersection, map snd intersection)
          -- Offset the right hand factor
          orh = [(l, index, i + length free1) | (l, index, i) <- rh]
          -- Generate contraction pairs
          pairs = zip lh orh
          -- Nest contractions
          nestedPairs = getNestedPairsNew pairs (length free1 + length free2)

getNestedPairsNew :: [ContractPairNew] -> Int -> [ContractPairNew]
getNestedPairsNew pairs n = newPairs
    where slots = [0..n]
          (_, newPairs) = foldr reduceNestedPairNew (slots, []) pairs

reduceNestedPairNew :: ContractPairNew -> ([Int], [ContractPairNew]) -> ([Int], [ContractPairNew])
reduceNestedPairNew ((l1, index1, i1), (l2, index2, i2)) (oldPos, newContractions)
    | i1 < i2 = (newPos, ((l1, index1, pos1), (l2, index2, pos2)):newContractions)
     where pos1 = unsafeMaybe $ elemIndex i1 oldPos
           pos2 = unsafeMaybe $ elemIndex i2 oldPos
           (spos1, spos2) = case pos2 > pos1 of
             True -> (pos1, pos2)
             False -> (pos2, pos1)
           (oldPos', _) = popAt spos2 oldPos
           (newPos, _) = popAt spos1 oldPos'

unsafeMaybe :: Maybe a -> a
unsafeMaybe (Just x) = x
unsafeMaybe _ = undefined

selfContractedIndexPairs :: [(Abs.Index, Index)] -> [ContractPairNew]
selfContractedIndexPairs idxs = nestedPairs
    where indexSlotPositions = zip idxs [0..length idxs]
          indexFormat = map (\((absindex, index), i) -> (indexLabel absindex, index, i)) indexSlotPositions
          distributedPairs = [
                (i1, i2) |
                i1@(_, _, x) <- indexFormat,
                i2@(_, _, y) <- indexFormat,
                x < y
           ]
          intersection = filter labelEqual distributedPairs
          nestedPairs = getNestedPairsNew intersection (length idxs)

indexTypeToIndex:: Abs.Index -> IndexType -> Index
indexTypeToIndex av ReprType{reprDim=d,reprGroup=g} = Index r v
            where r = ReprType d g
                  v = calcVal av
                  calcVal (Abs.Upper _) = Up
                  calcVal (Abs.Lower _) = Down

indexLabel :: Abs.Index -> String
indexLabel (Abs.Upper (Abs.Label lbl)) = lbl
indexLabel (Abs.Lower (Abs.Label lbl)) = lbl

data Transformation
    = Rewrite { rewriteMatch :: (->) Calc Bool, rewriteWith :: Calc }

compose :: Transformation
compose = undefined

-- data TensorMonad = undefined
execute :: String -> Calc -> Calc
execute "distribute" = distribute
execute "leibnitz" = leibnitz
execute "simpop" = simplifyOp
execute "simpf" = simplifyFactors
execute "simpnp" = simplifyN'
execute "simp" = simplify
execute "show" = showCalc
execute "sort" = sortCalc
execute _ = id

fixPoint :: (Calc -> Calc) -> Calc -> Calc
fixPoint f c
    | c' == c = c
    | otherwise = fixPoint f c'
    where c' = f c

showCalc :: Calc -> Calc
showCalc c = traceShowId c

-----------------------------------------------------------------------
-- Basic algebraic convenience
-----------------------------------------------------------------------

distribute :: Calc -> Calc
distribute = fixPoint distribute'

-- first order distribute
distribute' :: Calc -> Calc
distribute' c = case c of
    Prod (Sum s1 s2) fs -> Sum (Prod s1 fs) (Prod s2 fs)
    Prod fs (Sum s1 s2) -> Sum (Prod fs s1) (Prod fs s2)
    Prod f1 f2 -> Prod (distribute' f1) (distribute' f2)
    Sum s1 s2 -> Sum (distribute' s1) (distribute' s2)
    Permute p (Sum s1 s2) -> Sum (Permute p s1) (Permute p s2)
    Permute p c -> Permute p (distribute' c)
    Contract i1 i2 c -> Contract i1 i2 (distribute' c)
    _ -> c

simplifyOp :: Calc -> Calc
simplifyOp = fixPoint simplifyOp'

simplifyOp' :: Calc -> Calc
simplifyOp' (Op n idx (Permute p c)) = Permute (concatPermutations (identity (length idx)) p) (Op n idx c)
simplifyOp' (Op n idx (Contract i1 i2 c)) = Contract (i1 + length idx) (i2 + length idx) (Op n idx c)
simplifyOp' (Sum s1 s2) = Sum (simplifyOp s1) (simplifyOp s2)
simplifyOp' (Prod s1 s2) = Prod (simplifyOp s1) (simplifyOp s2)
simplifyOp' (Contract i1 i2 c) = Contract i1 i2 (simplifyOp c)
simplifyOp' (Permute p c) = Permute p (simplifyOp c)
simplifyOp' c = c

simplifyN :: Calc -> Calc
simplifyN = fixPoint simplifyN'

simplifyN' :: Calc -> Calc
simplifyN' (Prod (Number n) (Number m)) = Number (n*m)
simplifyN' (Permute p (Number n)) = Number n
simplifyN' (Prod f1 (Permute p (Prod (Number n) f2))) = Prod (Number n) (Prod f1 (Permute p f2))
simplifyN' (Prod (Permute p (Prod (Number n) f1)) f2) = Prod (Number n) (Prod (Permute p f1) f2)
simplifyN' (Prod f1 (Number n)) = Prod (Number n) (simplifyN' f1)
simplifyN' (Prod (Number n1) (Prod (Number n2) f2)) = Prod (Number (n1*n2)) (simplifyN' f2)
simplifyN' (Prod f1 (Prod (Number n) f2)) = Prod (Number n) (simplifyN' (Prod f1 f2))
simplifyN' (Prod (Prod (Number n) f1) f2) = Prod (Number n) (simplifyN' (Prod f1 f2))
simplifyN' (Prod f1 f2) = Prod (simplifyN' f1) (simplifyN' f2)
simplifyN' (Sum (Number n) (Number m)) = Number (n+m)
simplifyN' (Sum s1 (Number n)) = Sum (Number n) (simplifyN' s1)
simplifyN' (Sum s1 s2) = Sum (simplifyN' s1) (simplifyN' s2)
simplifyN' (Permute p c) = Permute p (simplifyN' c)
simplifyN' (Contract i1 i2 c) = Contract i1 i2 (simplifyN' c)
simplifyN' (Op n idx c) = Op n idx (simplifyN' c)
simplifyN' x = x


-- instance Ord Calc where
--   (Number _ ) <= (Tensor _ _) = True
--   t@(Tensor _ _) <= n@(Number _) = not (n <= t)
--   _ <= _ = undefined

-- i <= j -> i <= j
-- i <= t -> true
-- i * s <= t * j -> i <= j
-- i * s <= j * t -> i <= j
-- i + j
-- i <= t * j

-- Prod i (Prod t j)
-- Prod i*j t
-- i * (j * (k * (l * t))) <==> t * (i * ((k * j) * l))
--
simplify = simplifyPermutations . simplifyTerms . simplifyFactors . sortCalc

sortCalc :: Calc -> Calc
sortCalc = transform f
  where f s@(Sum s1 s2)
          | s1 <= s2 = s
          | otherwise = Sum s2 s1
        f p@(Prod p1 p2)
          | p1 <= p2 = p
          | otherwise = Prod p2 p1
        f x = x

simplifyFactors :: Calc -> Calc
simplifyFactors = transform simplifyFactors'

simplifyTerms :: Calc -> Calc
simplifyTerms = transform simplifyTerms'

simplifyPermutations :: Calc -> Calc
simplifyPermutations = transform simplifyPermutations'

simplifyFactors' :: Calc -> Calc
simplifyFactors' (Prod (Number n) (Number m)) = Number (n*m)
simplifyFactors' (Prod (Number m) (Prod (Number n) f)) = Prod (Number (n*m)) f
simplifyFactors' (Prod (Prod (Number n) f1) f2) = Prod (Number n) (Prod f1 f2)
simplifyFactors' x = x

simplifyTerms' :: Calc -> Calc
simplifyTerms' (Sum (Number n) (Number m)) = Number (n+m)
simplifyTerms' (Sum (Number m) (Sum (Number n) f)) = Sum (Number (n+m)) f
simplifyTerms' (Sum (Sum (Number n) f1) f2) = Sum (Number n) (Sum f1 f2)
simplifyTerms' x = x

simplifyPermutations' :: Calc -> Calc
simplifyPermutations' (Permute p (Prod (Number n) f)) = Prod (Number n) (Permute p f)
simplifyPermutations' (Permute p (Sum t1 t2)) = Sum (Permute p t1) (Permute p t2)
simplifyPermutations' x = x

-- a + a + a -> 3*a
collectTerms :: Abs.Expr -> Abs.Expr
collectTerms = undefined

-- a * a * a -> a^3
collectFactors :: Abs.Expr -> Abs.Expr
collectFactors = undefined

-- a + b, {b |-> b + c} -> a + b + c
substitute :: Abs.Expr -> Abs.Expr -> Abs.Expr -> Abs.Expr
substitute x m s = undefined

-- a*b -> a*b + b*a
symmetrize :: Abs.Expr -> [IndexType] -> Abs.Expr
symmetrize = undefined

antiSymmetrize :: Abs.Expr -> [IndexType] -> Abs.Expr
antiSymmetrize = undefined

expand :: Abs.Expr -> Abs.Expr
expand = undefined

sortTerms :: Abs.Expr -> Abs.Expr
sortTerms = undefined

-----------------------------------------------------------------------
-- Calculus
-----------------------------------------------------------------------

integrateByParts = undefined
differentiate = undefined
leibnitz :: Calc -> Calc
leibnitz = fixPoint leibnitz'

leibnitz' :: Calc -> Calc
leibnitz' (Op l idx (Prod f1 f2)) = Prod (Op l idx f1) f2 |+| Prod f1 (Op l idx f2)
leibnitz' (Op l idx c) = Op l idx (leibnitz c)
leibnitz' (Sum s1 s2) = Sum (leibnitz s1) (leibnitz s2)
leibnitz' (Permute p c) = Permute p (leibnitz c)
leibnitz' (Contract i1 i2 c) = Contract i1 i2 (leibnitz c)
leibnitz' c = c

-----------------------------------------------------------------------
-- Magic sauce
-----------------------------------------------------------------------

decompose :: Abs.Expr -> Abs.Expr
decompose = undefined

canonicalize :: Abs.Expr -> Abs.Expr
canonicalize = undefined

-----------------------------------------------------------------------
-- Curvature
-----------------------------------------------------------------------

riemann = undefined
ricci = undefined
weyl = undefined

