module Core where

-----------------------------------------------------------------------
-- Book analysis
-----------------------------------------------------------------------

import qualified Frontend.AbsTensor as Abs (
    Expr(..),
    Index(..),
    Label(..)
 )
import Data.Ratio
import qualified Data.Map as M
import Data.List
import Math.Combinat.Permutations
import qualified Tensor as T
import Util
import Control.Monad.Reader

import Debug.Trace

data IndexType = IndexType {
    indexDim :: Int,
    indexGroup :: GroupType,
    indexName :: String
}

instance Show IndexType where
    showsPrec i idx = showString (indexName idx)
        . (showParen True
        $ shows (indexDim idx))
        . showString ": "
        . shows (indexGroup idx)

data GroupType = GroupType {
    groupName :: String,
    groupDims :: [Int]
} deriving Eq

instance Show GroupType where
    showsPrec i g = showString (groupName g) . showList (groupDims g)

data TensorType = TensorType {
    tensorName :: String,
    tensorIndices :: [IndexType]
}

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
} deriving (Show, Eq)

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

emptyBook :: BookState
emptyBook = BookState [] [] [] $ M.fromList []

lookupTensor :: String -> BookState -> TensorType
lookupTensor l bs = head $ filter (\t -> tensorName t == l) $ bookTensors bs

lookupOp :: String -> BookState -> OpType
lookupOp l bs = head $ filter (\t -> opName t == l) $ bookOps bs

lookupCalc :: String -> BookState -> Maybe Calc
lookupCalc s bs = M.lookup s (bookCalcs bs)

-----------------------------------------------------------------------
-- Transformation
-----------------------------------------------------------------------

type TensorName = String
type OpName = String

data Calc
    = Sum Calc Calc
    | Prod Calc Calc
    | Transform String [Calc]
    | Power Int Calc
    | Permute Permutation Calc
    | Contract Int Int Calc
    | Number Rational
    | Tensor TensorName [Index]
    | Op OpName [Index] Calc
    deriving (Show, Eq)

data Index = Index {
    indexRepr :: ReprType,
    indexValence :: ValenceType
} deriving (Show, Eq)

data ValenceType = Up | Down deriving (Show, Eq)

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
indexToIndexType i = IndexType (reprDim $ indexRepr i) (reprGroup $ indexRepr i) "_"

calcFromExpr :: Abs.Expr -> Reader BookState (Calc, [(String, Index)])
calcFromExpr x = case x of
  Abs.Add expr1 expr2 -> do
    (calc1, idx1) <- calcFromExpr expr1
    (calc2, _) <- calcFromExpr expr2
    return (calc1 |+| calc2, idx1)
  Abs.Sub expr1 expr2 -> do
    (calc1, idx1) <- calcFromExpr expr1
    (calc2, _) <- calcFromExpr expr2
    return (calc1 |+| Number (-1) |*| calc2, idx1)
  Abs.Neg expr -> do
    (calc, idx) <- calcFromExpr expr
    return (Number (-1) |*| calc, idx)
  Abs.Div expr (Abs.Number num) -> do
    (calc, idx) <- calcFromExpr expr
    return (calc |*| Number (1 % num), idx)
  Abs.Div expr (Abs.Fraction p q) -> do
    (calc, idx) <- calcFromExpr expr
    return (calc |*| Number (p % q), idx)
  Abs.Div _ _ -> undefined
  Abs.Tensor (Abs.Label l) absIdx -> do
    maybeCalc <- asks (lookupCalc l)
    tensorType <- asks (lookupTensor l)
    let indexTypes = tensorIndices tensorType
    let freeSlots = T.freeIndexPos x
    let (indices, perm, contractions, sortedIdxAndLabels) = processIndexedObject absIdx indexTypes freeSlots
    let calc = case maybeCalc of
            Just storedCalc -> storedCalc
            Nothing -> (Tensor l indices)
    let contractedCalc = contractNew contractions calc
    return (Permute perm contractedCalc, sortedIdxAndLabels)
  Abs.Op (Abs.Label l) absIdx expr -> do
    (calc, idx) <- calcFromExpr expr
    opType <- asks (lookupOp l)
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
    (calc1, idx1) <- calcFromExpr expr1
    (calc2, idx2) <- calcFromExpr expr2
    let offset = length (T.freeIndexSlots expr1)
    let (perm, contracted, f') = processBinaryIndexed idx1 idx2 offset
    let res = (Permute perm $ contractNew contracted $ calc1 |*| calc2, f')
    return $ res
  Abs.Func (Abs.Label name) (expr:[]) -> do
    (calc, idx) <- calcFromExpr expr
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
indexTypeToIndex av IndexType{indexDim=d,indexGroup=g} = Index r v
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
execute "simpop" = simpOp
execute "simpn" = simpN
execute "simpnp" = simpN'
execute "show" = showCalc

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

simpOp :: Calc -> Calc
simpOp = fixPoint simpOp'

simpOp' :: Calc -> Calc
simpOp' (Op n idx (Permute p c)) = Permute (concatPermutations (identity (length idx)) p) (Op n idx c)
simpOp' (Op n idx (Contract i1 i2 c)) = Contract (i1 + length idx) (i2 + length idx) (Op n idx c)
simpOp' (Sum s1 s2) = Sum (simpOp s1) (simpOp s2)
simpOp' (Prod s1 s2) = Prod (simpOp s1) (simpOp s2)
simpOp' (Contract i1 i2 c) = Contract i1 i2 (simpOp c)
simpOp' (Permute p c) = Permute p (simpOp c)
simpOp' c = c

simpN :: Calc -> Calc
simpN = fixPoint simpN'

simpN' :: Calc -> Calc
simpN' (Prod (Number n) (Number m)) = Number (n*m)
simpN' (Permute p (Number n)) = Number n
simpN' (Prod f1 (Permute p (Prod (Number n) f2))) = Prod (Number n) (Prod f1 (Permute p f2))
simpN' (Prod (Permute p (Prod (Number n) f1)) f2) = Prod (Number n) (Prod (Permute p f1) f2)
simpN' (Prod f1 (Number n)) = Prod (Number n) (simpN' f1)
simpN' (Prod (Number n1) (Prod (Number n2) f2)) = Prod (Number (n1*n2)) (simpN' f2)
simpN' (Prod f1 (Prod (Number n) f2)) = Prod (Number n) (simpN' (Prod f1 f2))
simpN' (Prod (Prod (Number n) f1) f2) = Prod (Number n) (simpN' (Prod f1 f2))
simpN' (Prod f1 f2) = Prod (simpN' f1) (simpN' f2)
simpN' (Sum (Number n) (Number m)) = Number (n+m)
simpN' (Sum s1 (Number n)) = Sum (Number n) (simpN' s1)
simpN' (Sum s1 s2) = Sum (simpN' s1) (simpN' s2)
simpN' (Permute p c) = Permute p (simpN' c)
simpN' (Contract i1 i2 c) = Contract i1 i2 (simpN' c)
simpN' (Op n idx c) = Op n idx (simpN' c)
simpN' x = x

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

simplify :: Abs.Expr -> Abs.Expr
simplify = undefined

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

