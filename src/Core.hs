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

import Control.Monad.Reader
import Control.Monad.Except

import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.Ratio
import Data.Foldable

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
} deriving (Show, Eq, Ord)

-- instance Show GroupType where
--     showsPrec i g = showString (groupName g) . showList (groupDims g)

-- createRepr :: GroupType -> Int -> ReprType
-- createRepr = flip ReprType

data TensorType = TensorType {
    tensorName :: String,
    tensorIndices :: [IndexType]
} deriving (Show, Eq, Ord)

--instance Show TensorType where
--    showsPrec i t = showString (tensorName t)
--        . showString " { "
--        . showList (tensorIndices t)
--        . showString " }"

data OpType = OpType {
    opName :: String,
    opIndices :: [IndexType]
} deriving Show

-- instance Show OpType where
--     showsPrec i t = showString (opName t)
--         . showString " { "
--         . showList (opIndices t)
--         . showString " }"

data ReprType = ReprType {
    reprDim :: Int,
    reprGroup :: GroupType
    --reprMetric :: Maybe TensorType
} deriving (Show, Eq, Ord)

-- instance Show ReprType where
--     showsPrec i repr =
--         (showParen True
--         $ shows (reprDim repr))
--         . showString ": "
--         . shows (reprGroup repr)

data FunctionType = FunctionType {
    funcName :: String,
    funcArity :: Int
} deriving (Show, Eq)

data BookState = BookState {
    -- bookTensors :: M.Map TensorName TensorType,
    bookTensors :: [TensorType],
    bookOps :: [OpType],
    bookFuncs :: [FunctionType],
    bookCalcs :: M.Map String Calc,
    bookMetrics :: M.Map ReprType TensorType
    -- bookVariables :: [(String,Int)] let foo = a+b+c+d
} deriving Show

nextAnonymousId :: BookState -> Int
nextAnonymousId = length . filter (isPrefixOf "$") . M.keys . bookCalcs

nextAnonymous :: BookState -> String
nextAnonymous = ('$':) . show . nextAnonymousId

currentAnonymous :: BookState -> String
currentAnonymous = ('$':) . show . flip (-) 1 . nextAnonymousId

emptyBook :: BookState
emptyBook = BookState [] [] [] (M.fromList []) (M.fromList [])

lookupMetric' :: e-> ReprType -> BookState -> Either e TensorType
lookupMetric' e r = (maybeToEither e) . lookupMetric r

lookupMetric :: ReprType -> BookState -> Maybe TensorType
lookupMetric r bs = M.lookup r (bookMetrics bs)

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
    | Tensor TensorName [Index]
    | Prod Calc Calc
    | Sum Calc Calc
    | Transform String [Calc]
    | Power Int Calc
    | Permute Permutation Calc
    | Contract Int Int Calc
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
    --let indices = traceShow ((show i) ++ ":") $ traceShowId $ indexFromCalc c
    let indices = indexFromCalc c
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

otherValence :: ValenceType -> ValenceType
otherValence Up = Down
otherValence Down = Up

indexFromCalc :: Calc -> [Index]
indexFromCalc x = case x of
  (Number _) -> []
  (Power _ _) -> []
  (Tensor _ []) -> []
  (Tensor _ idx) -> idx
  (Sum first _) -> indexFromCalc first
  (Prod f1 f2) -> indexFromCalc f1 ++ indexFromCalc f2
  -- Permuted indices must be of same type so can just pass through
  (Permute p c) -> permuteList p $ indexFromCalc c
  (Contract i1 i2 c)
      | i1 < i2 -> deleteAt i1 $ deleteAt i2 (indexFromCalc c)
  (Contract i1 i2 c)
      | i1 > i2 -> deleteAt i2 $ deleteAt i1 (indexFromCalc c)
  (Op n idx c) -> idx ++ indexFromCalc c

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
indexToIndexType i = indexRepr i
--indexToIndexType i = ReprType (reprDim $ indexRepr i) (reprGroup $ indexRepr i)

type Error = Except String

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e (Nothing) = Left e
maybeToEither _ (Just x)  = Right x


data Transformation
    = Rewrite { rewriteMatch :: (->) Calc Bool, rewriteWith :: Calc }

compose :: Transformation
compose = undefined

-- data TensorMonad = undefined

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


