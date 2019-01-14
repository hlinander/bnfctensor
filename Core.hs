module Core where

-----------------------------------------------------------------------
-- Book analysis
-----------------------------------------------------------------------

import qualified Frontend.AbsTensor as Abs ( 
    Expr(..), 
    Label(..) 
 )
import Data.Ratio
import Math.Combinat.Permutations

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
    bookTensors :: [TensorType],
    bookFuncs :: [FunctionType]
} deriving Show

-----------------------------------------------------------------------
-- Transformation
-----------------------------------------------------------------------

data Calc
    = Op Operation [Calc]
    | Number Rational
    | Tensor String [IndexType] Permutation
    -- | Tensor String [IndexType] Permutation

-- T.a.b.c, T.b.a.c, T^a.b.c
-- T^a.a.b ~ contract(T^a.b.c, {a,b})

data Operation
    = (:+)
    | (:*)
    | Transform String
    | Power Int
    -- | Contract Int Int -- Index position in expression (note behaviour in terms/factors)
    -- | Contract String String -- Index labels
    | Call { func :: (->) Calc Calc }

-- Index positions is fragile against tree modifications under the contraction
-- tensor T { a(2), b(2): SO(3) }
-- symmatric T
-- -- Contract index .a and .c in the expression below
-- contract(T.a.b T.c.d, [1, 3])
-- -- Now use the symmetry in T to swap .a and .b, how should the contraction be kept up to date?
-- -- Compute-wise the natural thing would be to move the contraction to
-- contract(T.b.a T.c.d, [2, 3])

-- Index labels are more flexible and naturally handles the above case but gives rise to
-- the unatural usage of specific labels and extra logic when traversing.

calcFromExpr :: BookState -> Abs.Expr -> Calc
calcFromExpr bs x = case x of
  Abs.Func (Abs.Label label) exprs -> Op (Transform label) (map recurse exprs)
  Abs.Add expr1 expr2 -> Op (:+) [recurse expr1, recurse expr2]
  Abs.Sub expr1 expr2 -> Op (:+) [recurse expr1, Op (:*) [Number (-1), recurse expr2]]
  Abs.Neg expr -> Op (:*) [Number (-1), recurse expr]
  Abs.Mul expr1 expr2 -> Op (:*) [recurse expr1, recurse expr2]
  Abs.Div expr1 (Abs.Number num) -> Op (:*) [recurse expr1, Number (1 % num)]
  Abs.Div expr1 (Abs.Fraction p q) -> Op (:*) [recurse expr1, Number (p % q)]
  Abs.Div expr1 _ -> undefined
  -- Abs.Indexed (Abs.Tensor (Abs.Label label)) indices -> Tensor label $ indexTypes bs x
  -- Abs.Indexed expr indices -> 
  Abs.Tensor (Abs.Label label) indices -> Tensor label 
  Abs.Number p -> Number (fromInteger p)
  Abs.Fraction p q -> Number (p % q)
  where recurse = calcFromExpr bs

data Index = Index { 
    indexRepr :: ReprType,
    indexValence :: ValenceType 
} deriving Show

data ValenceType = Up | Down deriving Show

getIndices :: BookState -> String -> [Abs.Index] -> [Index]
getIndices bs label indices = map (getIndex bs label) indices

getIndex bs label index = findIndex 


indexTypes :: BookState -> Abs.Expr -> [IndexType]
indexTypes bs x = case x of
  Abs.Func (Abs.Label label) exprs -> undefined
  Abs.Add expr1 expr2 -> recurse expr1
  Abs.Sub expr1 expr2 -> recurse expr1
  Abs.Neg expr -> recurse expr
  Abs.Mul expr1 expr2 -> recurse expr1 ++ recurse expr2
  Abs.Div expr1 (Abs.Number num) -> []
  Abs.Div expr1 (Abs.Fraction p q) -> []
  -- Abs.Indexed expr indices -> return $ IndexType 0 (GroupType "" [0]) ""
  Abs.Tensor (Abs.Label label) indices -> 
  -- Abs.Number p -> Number (fromInteger p)
  -- Abs.Fraction p q -> Number (p % q)
  where recurse = indexTypes bs

-- (-1)*T.a
-- -T.a
-- Func (:-) [Indexed (Atom (Tensor "T")) [Lower "a"]]
-- (A.a C^a^c) * (A.a D^a^c)

data Transformation
    = Rewrite { rewriteMatch :: (->) Calc Bool, rewriteWith :: Calc }

-- a(b+c) -> ab ac
-- distribute = Distr (:*) (:+)
-- collectTerms = Reduce (:+)
-- d(ab) = (da)b + a(db)
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

