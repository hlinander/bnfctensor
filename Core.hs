module Core where

-----------------------------------------------------------------------
-- Book analysis
-----------------------------------------------------------------------

import Frontend.AbsTensor ( Expr, Book )

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
}

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
    = Func Operation [Calc]
    | Power Calc Int
    | Cont Calc [IndexType]
    | Atom Atom
    -- deriving (Eq)

data Atom
    = Number Rational
    | Tensor String
    -- deriving (Eq)

data Operation
    = Sum
    | Product
    | Neg
    | Transform String
    | Call { func :: (->) Calc Calc }
    -- deriving (Eq)
    -- | Div

data Transformation
    = Rewrite { rewriteMatch :: (->) Calc Bool, rewriteWith :: Calc }


compose :: Transformation
compose = undefined


-- data TensorMonad = undefined

-----------------------------------------------------------------------
-- Basic algebraic convenience
-----------------------------------------------------------------------

eval :: Calc -> BookState -> Book
eval = undefined

-- a*(b+c) -> a*b + a*c
distribute :: Expr -> Expr
distribute = undefined

-- a + a + a -> 3*a
collectTerms :: Expr -> Expr
collectTerms = undefined

-- a * a * a -> a^3
collectFactors :: Expr -> Expr
collectFactors = undefined

-- a + b, {b |-> b + c} -> a + b + c
substitute :: Expr -> Expr -> Expr -> Expr
substitute x m s = undefined

-- a -> a - a
symmetrize :: Expr -> Expr
symmetrize = undefined

antiSymmetrize :: Expr -> Expr
antiSymmetrize = undefined

expand :: Expr -> Expr
expand = undefined

sortTerms :: Expr -> Expr
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

simplify :: Expr -> Expr
simplify = undefined

decompose :: Expr -> Expr
decompose = undefined

canonicalize :: Expr -> Expr
canonicalize = undefined

-----------------------------------------------------------------------
-- Curvature
-----------------------------------------------------------------------

riemann = undefined
ricci = undefined
weyl = undefined

