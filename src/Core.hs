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
import Control.Monad.Fix

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
    bookFuncs :: [FunctionType],
    bookCalcs :: M.Map String Calc
    -- bookVariables :: [(String,Int)] let foo = a+b+c+d
} deriving Show

emptyBook :: BookState
emptyBook = BookState [] [] $ M.fromList []

lookupTensor :: String -> BookState -> TensorType
lookupTensor l bs = head $ filter (\t -> tensorName t == l) $ bookTensors bs

lookupCalc :: String -> BookState -> Maybe Calc
lookupCalc s bs = M.lookup s (bookCalcs bs)

-----------------------------------------------------------------------
-- Transformation
-----------------------------------------------------------------------

type TensorName = String

data Calc
    = Sum [Calc]
    | Prod [Calc]
    | Transform String [Calc]
    | Power Int Calc
    | Permute Permutation Calc
    | Contract Int Int Calc
    | Number Rational
    | Tensor TensorName [Index]
    | Variable String
    -- | Func String [Calc]
    deriving (Show, Eq)

-- data Function
--     = Zero (BookState -> *)
--     | Succ (* -> *) Function
--
-- evalF :: Function -> BookState -> a
-- evalF (Succ f (Zero f')) bs = f $ f' bs
-- evalF (Succ f fs) bs = f . evalF fs bs
--
-- instance Show Function where
--     showsPrec _ _ = showChar ' '

data Index = Index {
    indexRepr :: ReprType,
    indexValence :: ValenceType
} deriving (Show, Eq)

data ValenceType = Up | Down deriving (Show, Eq)

infixl 5 |*|
infixl 4 |+|

(|*|) :: Calc -> Calc -> Calc
c1 |*| c2 = Prod [c1, c2]

(|+|) :: Calc -> Calc -> Calc
c1 |+| c2 = Sum [c1, c2]

tensorTypeFromCalc :: String -> Calc -> TensorType
tensorTypeFromCalc l c = TensorType l (indexTypeFromCalc c)

indexTypeFromCalc :: Calc -> [IndexType]
indexTypeFromCalc (Tensor _ []) = []
indexTypeFromCalc (Tensor _ idx) = map indexToIndexType idx
indexTypeFromCalc (Sum []) = []
indexTypeFromCalc (Sum (first:_)) = indexTypeFromCalc first
indexTypeFromCalc (Prod []) = []
indexTypeFromCalc (Prod factors) = concat (map indexTypeFromCalc factors)
-- Permuted indices must be of same type so can just pass through
indexTypeFromCalc (Permute p c) = indexTypeFromCalc c
indexTypeFromCalc (Contract i1 i2 c)
    | i1 < i2 = deleteAt i1 $ deleteAt i2 (indexTypeFromCalc c)

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
    let absIndicesWithType = zip absIdx $ tensorIndices tensorType :: [(Abs.Index, IndexType)]
    let indices = map (uncurry indexTypeToIndex) absIndicesWithType :: [Index]
    -- Which of the indices are free?
    let freeSlots = T.freeIndexPos x :: [(Abs.Index, Int)]
    let freeSlotsPos = map snd freeSlots :: [Int]
    let freeSlotsLabels = map (indexLabel.fst) freeSlots :: [String]
    let freeIndices = map (indices!!) freeSlotsPos :: [Index]
    let freeIndicesAndLabels = zip freeSlotsLabels freeIndices :: [(String, Index)]
    let sortedIdxAndLabels = sortBy (\(a, _) (b, _) -> compare a b) freeIndicesAndLabels
    let perm = inverse $ sortingPermutationAsc freeSlotsLabels
    let contractions = selfContractedIndexPairs (zip absIdx indices)
    let calc = case maybeCalc of
            Just storedCalc -> storedCalc
            Nothing -> (Tensor l indices)
    let contractedCalc = contractNew contractions calc
    return (Permute perm contractedCalc, sortedIdxAndLabels)
  Abs.Number p -> return (Number (fromInteger p), [])
  Abs.Fraction p q -> return (Number (p % q), [])
  Abs.Mul expr1 expr2 -> do
    (calc1, idx1) <- calcFromExpr expr1
    (calc2, idx2) <- calcFromExpr expr2
    let pairs = contractedPairsNew idx1 idx2
    let offset = length (T.freeIndexSlots expr1)
    let c1 = map (\(_,_,i) -> i) $ map fst pairs
    let c2 = map (\(_,_,i) -> i) $ map snd pairs
    let c2' = map (\i -> i - offset) c2
    let f1 = foldr deleteAt idx1 c1
    let f2 = foldr deleteAt idx2 c2'
    let f = map fst $ f1 ++ f2
    let perm = inverse $ sortingPermutationAsc f
    let f' = permuteList (inverse $ perm) (f1 ++ f2)
    let res = (Permute perm $ contractNew pairs $ calc1 |*| calc2, f')
    return $ res
  Abs.Func (Abs.Label "distribute") (expr:[]) -> do
    (calc, idx) <- calcFromExpr expr
    return $ (distribute calc, idx)

  x -> (return $ (traceShow ("vinsten: " ++ show x) (Number 1), []))

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

-- a(b+c) -> ab ac
-- distribute = Distr (:*) (:+)
-- collectTerms = Reduce (:+)
-- leibnitz = Distr (:+) (:*)
-- Rewrite (\c -> )
-- matchDist (Times )


compose :: Transformation
compose = undefined


-- data TensorMonad = undefined
execute :: String -> Calc -> Calc
execute "distribute" = distribute

-- execute func calc = func calc
--     where
--     func = undefined
--     tensorFunctions = [
--         ("distribute", distribute)
--      ]



-----------------------------------------------------------------------
-- Basic algebraic convenience
-----------------------------------------------------------------------

-- eval :: Monad m => m Calc -> BookState -> Book
eval = undefined

 -- Book
 -- let yo = T^...
 -- let dyo = distribute(yo)
 -- let dyo2 = canonicalize(dyo)

-- a*(b+c)*d*(e+f*(g+h)) -> a*b + a*c
-- a*(b+c) -> a*b + a*c
distribute :: Calc -> Calc
distribute = distribute'

-- (a + b) T = +[*[a,T],  
-- s := a + b
-- cs := T
-- 
-- (a + b)(c + d)(e + f)(g + h)(i + j)
-- gren1
-- s := (a+b)
-- cs := [c, d]
-- gren2
-- s := c+d
-- cs := [a,b]
-- -> +[*[a,+[c,d]], *[b, +[c,d]]]
-- \x -> \x -> \x -> \x -> x 
-- Permute (toPermutation []) (Prod [Permute (toPermutation []) (Tensor "Q" []),Sum [Permute (toPermutation []) (Tensor "Q" []),Permute (toPermutation []) (Tensor "Q" [])]])


-- first order distribute
distribute' :: Calc -> Calc
distribute' c = case c of
    Prod (Sum cs:fs) -> Sum (map (\x -> distribute' $ Prod (x:fs)) cs)
    Prod [f, Sum cs] -> Sum (map (\x -> distribute' $ Prod (f:[x])) cs)
    Prod [_] -> c
    Prod (f:fs) -> Prod [f, distribute' (Prod fs)]
    Sum cs -> Sum $ map distribute' cs
    Permute p c -> Permute p (distribute' c)
    Contract i1 i2 c -> Contract i1 i2 (distribute' c)
    _ -> c

--recurseCalc :: (Calc -> Calc) -> Calc -> Calc
--recurseCalc f (Sum terms) = Sum (map f terms)
--recurseCalc f (Prod factors) = Prod (map f factors)
--recurseCalc f x = f x

flattenCalc ::Calc -> Calc
flattenCalc c = case c of
    Prod [] -> Number 1
    Prod [Prod fs] -> Prod $ recurse fs
    Prod [f, Prod fs] -> Prod (f:recurse fs)
    Prod fs -> Prod $ recurse fs
    Sum [] -> Number 0
    Sum [Sum ts] -> Sum $ recurse ts
    Sum [t, Sum ts] -> Sum (t:recurse ts)
    Sum ts -> Sum $ recurse ts
    Permute p c -> Permute p (flattenCalc c)
    Contract i1 i2 c -> Contract i1 i2 (flattenCalc c)
    _ -> c
    where recurse = map flattenCalc

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
leibnitz = undefined

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

