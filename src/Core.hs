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
import Tensor
import Util
import Control.Monad.Reader

import Debug.Trace
import Frontend.PrintTensor

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
}

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
    reprDim :: Int
} deriving Show

data FunctionType = FunctionType {
    funcName :: String,
    funcArity :: Int
} deriving Show

data BookState = BookState {
    -- bookTensors :: M.Map TensorName TensorType,
    bookTensors :: [TensorType],
    bookFuncs :: [FunctionType],
    bookCalcs :: [Calc]
    -- bookVariables :: [(String,Int)] let foo = a+b+c+d
} deriving Show

emptyBook :: BookState
emptyBook = BookState [] [] []

lookupTensor :: String -> BookState -> TensorType
lookupTensor l bs = head $ filter (\t -> tensorName t == l) $ bookTensors bs

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
    deriving Show

data Index = Index {
    indexRepr :: ReprType,
    indexValence :: ValenceType
} deriving Show

data ValenceType = Up | Down deriving Show

infixl 5 |*|
infixl 4 |+|

(|*|) :: Calc -> Calc -> Calc
c1 |*| c2 = Prod [c1, c2]

(|+|) :: Calc -> Calc -> Calc
c1 |+| c2 = Sum [c1, c2]

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
    tensorType <- asks (lookupTensor l)
    let indices = zip absIdx $ tensorIndices tensorType
    let indices' = map (uncurry indexTypeToIndex) indices
    let freeSlots = freeIndexPos x
    let freeSlotsPos = map snd freeSlots
    let freeSlotsLabels = map (indexLabel.fst) freeSlots
    let freeIndices' = map (indices'!!) freeSlotsPos
    let freeIndicesAndLabels = zip freeSlotsLabels freeIndices'
    let sortedIdxAndLabels = sortBy (\(a, _) (b, _) -> compare a b) freeIndicesAndLabels
    let perm = inverse $ sortingPermutationAsc freeSlotsLabels
    calc <- selfContractions x
    return (Permute perm calc, sortedIdxAndLabels)
  Abs.Number p -> return (Number (fromInteger p), [])
  Abs.Fraction p q -> return (Number (p % q), [])
  Abs.Mul expr1 expr2 -> do
    (calc1, idx1) <- calcFromExpr expr1
    (calc2, idx2) <- calcFromExpr expr2
    let pairs = contractedPairs expr1 expr2
    let c1 = map (snd.fst) pairs
    let c2 = map (snd.snd) pairs
    let f1 = foldr deleteAt idx1 c1
    let f2 = foldr deleteAt idx2 c2
    let f = map fst $ f1 ++ f2
    let perm = inverse $ sortingPermutationAsc f
    let f' = permuteList perm (f1 ++ f2)
    return (Permute perm $ contract pairs $ calc1 |*| calc2, f')

  _ -> undefined

-- Create nested contractions for a list of contraction pairs of slots
contract :: [ContractPair] -> Calc -> Calc
contract [] expr = expr
contract (((_, i1), (_, i2)):rest) expr = Contract i1 i2 $ contract rest expr

type IndexSlot = (Abs.Index, Int)
type ContractPair = (IndexSlot, IndexSlot)

labelEqual :: ContractPair -> Bool
labelEqual ((l1, _),(l2, _)) = indexLabel l1 == indexLabel l2

contractedPairs:: Abs.Expr -> Abs.Expr -> [ContractPair]
contractedPairs expr1 expr2 = nestedPairs
    where -- Get intersecting pairs of labels
          free1 = freeIndexSlots expr1
          free2 = freeIndexSlots expr2
          cartProd = [(i1, i2) | i1 <- free1, i2 <- free2]
          intersection = filter labelEqual cartProd
          -- Offset the right hand factor
          (lh, rh) = (map fst intersection, map snd intersection)
          orh = offsetIndices free1 rh
          -- Generate contraction pairs
          pairs = zip lh orh :: [(IndexSlot, IndexSlot)]
          -- Nest contractions
          nestedPairs = getNestedPairs pairs (length free1 + length free2)
          -- [2,3][0,1]{0}{1}{2}{3}
          -- [2,3]{2}{3}
getNestedPairs :: [ContractPair] -> Int -> [ContractPair]
getNestedPairs pairs n = newPairs
    where slots = [0..n]
          (_, newPairs) = foldr reduceNestedPair (slots, []) pairs

reduceNestedPair :: ContractPair -> ([Int], [ContractPair]) -> ([Int], [ContractPair])
reduceNestedPair ((index1, i1), (index2, i2)) (oldPos, newContractions)
    | i1 < i2 = (newPos, ((index1, pos1), (index2, pos2)):newContractions)
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

selfContractions :: Abs.Expr -> Reader BookState Calc
selfContractions (Abs.Tensor (Abs.Label l) idxs) = do
        tensorType <- asks (lookupTensor l)
        let indices = zipWith indexTypeToIndex idxs (tensorIndices tensorType)
        return $ contract (contractedIndexPairs idxs) (Tensor l indices)
selfContractions _ = undefined

contractedIndexPairs :: [Abs.Index] -> [ContractPair]
contractedIndexPairs idxs = filter labelEqual distributedPairs
    where indexSlotPositions = zip idxs [0..length idxs]
          distributedPairs = [
                (i1, i2) |
                i1@(_,x) <- indexSlotPositions,
                i2@(_,y) <- indexSlotPositions,
                i1 /= i2 && x < y
           ]

indexTypeToIndex:: Abs.Index -> IndexType -> Index
indexTypeToIndex av IndexType{indexDim=d} = Index r v
            where r = ReprType d
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
distribute = flattenCalc . distribute'

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

-- first order distribute
distribute' :: Calc -> Calc
distribute' c = case c of
    Prod (Sum cs:fs) -> Sum (map (\x -> distribute' $ Prod (x:fs)) cs)
    Prod [f, Sum cs] -> Sum (map (\x -> distribute' $ Prod (f:[x])) cs)
    Prod (f:fs) -> Prod [f, distribute' (Prod fs)]
    Sum cs -> Sum $ map distribute' cs
    _ -> c

flattenCalc ::Calc -> Calc
flattenCalc c = case c of
    Prod [Prod fs] -> Prod $ recurse fs
    Prod [f, Prod fs] -> Prod (f:recurse fs)
    Prod fs -> Prod $ recurse fs
    Sum [Sum ts] -> Sum $ recurse ts
    Sum [t, Sum ts] -> Sum (t:recurse ts)
    Sum ts -> Sum $ recurse ts
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

