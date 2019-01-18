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
import Control.Monad.Reader

data IndexType = IndexType {
    indexDim :: Int,
    indexGroup :: GroupType,
    indexName :: String
} deriving Show

data GroupType = GroupType {
    groupName :: String,
    groupDims :: [Int]
} deriving Show

data TensorType = TensorType {
    tensorName :: String,
    tensorIndices :: [IndexType]
} deriving Show

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
    bookFuncs :: [FunctionType]
} deriving Show

emptyBook = BookState [] []

lookupTensor :: String -> BookState -> TensorType
lookupTensor l bs = head $ filter (\t -> tensorName t == l) $ bookTensors bs

-----------------------------------------------------------------------
-- Transformation
-----------------------------------------------------------------------

type TensorName = String

data Calc
    = Op Operation [Calc] -- [Int] TODO(maybe????): List of free slots
    | Number Rational
    | Tensor TensorName [Index] Permutation
    deriving Show
    -- | Component TensorName [ComponentIndex] Permutation

data Index = Index { 
    indexRepr :: ReprType,
    indexValence :: ValenceType
} deriving Show

data ValenceType = Up | Down deriving Show

data Operation
    = (:+)
    | (:*)
    | Transform String
    | Power Int
    | Contract Int Int -- Index position in expression (note behaviour in terms/factors)
    -- | Call { func :: (->) Calc Calc }
    deriving Show

-- contract(D.d(T.a.b) S.c[contract(M.e.f.c, {e,f})], {d,c})
-- contract(D.d(T.a.b) S.c, [1, 4])
-- contract(D.d(T.a.b) contract(M.e.f.c, [1, 2]), [1, 4])

-- D.d(T.a.b) M.e.f.c
-- free: [2 3]
-- dummy: [1 7 4 5]

-- T.a.b.c + T.c.b.a
-- T ()      T (1 3)

-- let expr = S^a T.a.b.c + S^a T.c.b.a
-- T ()      T (1 3)
-- contract(S.d T.a.b.c, [1, 2]) + contract(S.d T.a.b.c{(1,3)}, [1, 4])

-- symmetrize(expr, {b, c})
-- ... + sym(contract(S.d T.a.b.c{(1,3)}, [1, 4]), [2, 3])



-- Index positions is fragile against tree modifications under the contraction
-- tensor T { a(2), b(2): SO(3) }
-- symmatric T
-- -- Contract index .a and .c in the expression below
-- contract(T.a.b T.c.d, [1, 3]) ~ T.a.b T^a.d
-- -- Now use the symmetry in T to swap .a and .b, how should the contraction be kept up to date?
-- -- Compute-wise the natural thing would be to move the contraction to
-- contract(T.b.a T.c.d, [2, 3])
-- 

-- Index labels are more flexible and naturally handles the above case but gives rise to
-- the unatural usage of specific labels and extra logic when traversing.

infixl 5 |*|
infixl 4 |+|

(|*|) :: Calc -> Calc -> Calc
c1 |*| c2 = Op (:*) [c1, c2]  

(|+|) :: Calc -> Calc -> Calc
c1 |+| c2 = Op (:+) [c1, c2]  

calcFromExpr :: Abs.Expr -> Reader BookState Calc
calcFromExpr x = case x of
  -- Abs.Func (Abs.Label label) exprs -> Op (Transform label) (map recurse exprs)
  -- [recurse expr1, recurse expr2]
  -- [a]
  Abs.Add expr1 expr2 -> Op (:+) <$> mapM calcFromExpr [expr1, expr2]
  -- Abs.Sub expr1 expr2 -> Op (:+) <$> [recurse expr1, Op (:*) [Number (-1), recurse expr2]]
  Abs.Sub expr1 expr2 -> do
    calc1 <- calcFromExpr expr1
    calc2 <- calcFromExpr expr2
    return $ calc1 |+| Number (-1) |*| calc2
  Abs.Neg expr -> do
    calc <- calcFromExpr expr
    return $ Number (-1) |*| calc
  Abs.Div expr (Abs.Number num) -> do
    calc <- calcFromExpr expr
    return $ calc |*| Number (1 % num)
  Abs.Div expr (Abs.Fraction p q) -> do
    calc <- calcFromExpr expr
    return $ calc |*| Number (p % q)
  Abs.Div expr1 _ -> undefined
  -- -- Abs.Indexed (Abs.Tensor (Abs.Label label)) indices -> Tensor label $ indexTypes bs x
  -- -- Abs.Indexed expr indices -> 
  -- -- T.a.b.c  + ...
  -- -- T.a^a.c -> Cont  
  Abs.Tensor (Abs.Label label) indices -> selfContractions x
  Abs.Number p -> return $ Number (fromInteger p)
  Abs.Fraction p q -> return $ Number (p % q)
  Abs.Mul expr1 expr2 -> do
    calc1 <- calcFromExpr expr1
    calc2 <- calcFromExpr expr2
    return (contract pairs $ calc1 |*| calc2)
        where pairs = contractedPairs expr1 expr2

-- Create nested contractions for a list of contraction pairs of slots
contract :: [ContractPair] -> Calc -> Calc
contract [] expr = expr
contract (((_, i1), (_, i2)):rest) expr = Op (Contract i1 i2) [contract rest expr]

type IndexSlot = (Abs.Index, Int)
type ContractPair = (IndexSlot, IndexSlot)

labelEqual :: ContractPair -> Bool
labelEqual ((l1, i1),(l2, i2)) = indexLabel l1 == indexLabel l2

contractedPairs:: Abs.Expr -> Abs.Expr -> [ContractPair]
contractedPairs expr1 expr2 = pairs
    where -- Get intersecting pairs of labels
          cartProd = [(i1, i2) | i1 <- (freeIndexSlots expr1), i2 <- (freeIndexSlots expr2)]
          intersection = filter labelEqual cartProd
          -- Offset the right hand factor
          (lh, rh) = (map fst intersection, map snd intersection)
          orh = offsetIndices lh rh
          -- Generate contraction pairs
          pairs = zip lh orh

tensorLabelPermution :: Abs.Expr -> Permutation
tensorLabelPermution (Abs.Tensor _ idxs) = inverse $ sortingPermutationAsc (map indexLabel idxs)

-- generates calc contractions from tensor
selfContractions :: Abs.Expr -> Reader BookState Calc
selfContractions t@(Abs.Tensor (Abs.Label l) idxs) = do
        tensorType <- asks (lookupTensor l)
        let indices = map (uncurry indexTypeToIndex) $ zip idxs (tensorIndices tensorType)
        return $ contract intersection (Tensor l indices $ tensorLabelPermution t)
    where indexSlots = zip idxs [0..length idxs]
          cartProd = [(i1, i2) | i1 <- indexSlots, i2 <- indexSlots, not $ i1 == i2]
          intersection = filter labelEqual cartProd

indexTypeToIndex:: Abs.Index -> IndexType -> Index
indexTypeToIndex av IndexType{indexDim=d} = Index r v
            where r = ReprType d
                  v = calcVal av
                  calcVal (Abs.Upper _) = Up
                  calcVal (Abs.Lower _) = Down

indexLabel :: Abs.Index -> String
indexLabel (Abs.Upper (Abs.Label lbl)) = lbl
indexLabel (Abs.Lower (Abs.Label lbl)) = lbl

getIndices :: BookState -> String -> [Abs.Index] -> [Index]
getIndices bs t indices = map (getIndex bs t) indices

getIndex bs label index = undefined 

-- (-1)*T.a
-- -T.a
-- Func (:-) [Indexed (Atom (Tensor "T")) [Lower "a"]]
-- (A.a C^a^c) * (A.a D^a^c)

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

-----------------------------------------------------------------------
-- Basic algebraic convenience
-----------------------------------------------------------------------

-- eval :: Monad m => m Calc -> BookState -> Book
eval = undefined 

 -- Book
 -- let yo = T^...
 -- let dyo = distribute(yo)
 -- let dyo2 = canonicalize(dyo)




-- a*(b+c) -> a*b + a*c
distribute :: Abs.Expr -> Abs.Expr
distribute = undefined

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

