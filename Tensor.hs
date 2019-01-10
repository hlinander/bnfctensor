module Main where

{-# LANGUAGE BangPatterns #-}

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import Frontend.LexTensor ( Token, tokens )
import Frontend.ParTensor ( pBook )
import Frontend.PrintTensor
import Frontend.AbsTensor
import Frontend.ErrM

import System.IO.Unsafe

import Control.Monad
import Control.Monad.State
import Data.List

type ParseFun a = [Token] -> Err a

type Result = Err Book

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

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

analyzeBook :: Book -> State BookState ()
analyzeBook (Derivation ss) = mapM_ analyzeStmt ss

analyzeStmt :: Stmt -> State BookState ()
analyzeStmt stmt = case stmt of
    StmtTensorDef labels tensordef -> analyzeTensorDef labels tensordef
    StmtFuncDef ident exprs stmts -> funcAppend $ FunctionType (labelFromIdent ident) (length exprs)
    StmtAssign label expr -> analyzeExpr expr
    StmtVoid expr -> analyzeExpr expr
    _ -> return ()

analyzeTensorDef :: [LabelList] -> [TensorDef] -> State BookState ()
analyzeTensorDef lls def = mapM_ tensorAppend (map tensor labels)
    where labels = map labelsFromList lls
          tensor label = TensorType label (analyzeIndices def)

analyzeIndex :: TensorDef -> [IndexType]
analyzeIndex (TensorDef indices (GroupDef (Label gl) nums)) = map indexType indices
    where group = GroupType gl $ numsToInts nums
          indexType (IndexGroup (Label il) idim) = IndexType (fromInteger idim) group il

analyzeIndices :: [TensorDef] -> [IndexType]
analyzeIndices defs = concatMap analyzeIndex defs

analyzeExpr :: Expr -> State BookState ()
analyzeExpr expr = case expr of
    -- Indexed expr indices -> undefined
    Add e1 e2 -> case sort (freeIndices e1) == sort (freeIndices e2) of
        True -> return ()
        False -> undefined
    Tensor label -> checkTensorDecl label
    _ -> return ()

freeIndices :: Expr -> [Index]
freeIndices x = freeIndices_ x []

freeIndices_ x s = case x of
        -- in s, in indices or free
    Indexed _ indices -> filter isFree indices
        where isFree index = not (indexLabelIn index s || occurences index indices > 1)
              occurences x list = length $ filter (valenceFreeEq x) list
    Func label exprs -> undefined
    Add expr1 expr2 -> freeIndices_ expr1 s
    Sub expr1 expr2 -> freeIndices_ expr1 s
    Neg expr -> freeIndices_ expr s
    Mul expr1 expr2 -> freeIndices_ expr1 (s ++ freeIndices_ expr2 s) ++
                       freeIndices_ expr2 (s ++ freeIndices_ expr1 s)
    Div expr1 expr2 -> freeIndices expr1
    Tensor label -> undefined
    Number integer -> []
    Fraction integer1 integer2 -> []

valenceFreeEq :: Index -> Index -> Bool
valenceFreeEq (Upper a) (Lower b) = a == b
valenceFreeEq (Lower a) (Upper b) = a == b
valenceFreeEq (Lower a) (Lower b) = a == b
valenceFreeEq (Upper a) (Upper b) = a == b

indexLabelIn :: Index -> [Index] -> Bool
indexLabelIn index list = any (valenceFreeEq index) list 

checkTensorDecl :: Label -> State BookState ()
checkTensorDecl (Label s) = do
    tensorType <- findDeclTensor s
    case tensorType of
        -- [] -> haxxorPrint ("Tensor '" ++ s ++ "' not declared") >> return ()
        ((TensorType name dim) : []) -> return ()
        _ -> undefined
        --(t : ts) -> haxxorPrint ("Tensor '" ++ s ++ "' declared multiple times") >> return ()
    -- let tensorLabels = map tensorName (findDeclTensor s)
    -- case s `elem` tensorLabels of
        -- True -> return ()
        -- False -> return $ haxxorPrint $ "Tensor '" ++ s ++ "' not declared"

checkTensorDim :: Label -> State BookState ()
checkTensorDim (Label s) = do
    bookState <- get
    let tensorLabels = map tensorName (bookTensors bookState)
    case s `elem` tensorLabels of
        True -> return ()
        False -> undefined
        -- False -> return $ haxxorPrint $ "Tensor '" ++ s ++ "' not declared"

findDeclTensor :: String -> State BookState [TensorType]
findDeclTensor s = do
    bookState <- get
    return $ filter (\t -> tensorName t == s) (bookTensors bookState)

-- Func label exprs -> failure x
-- Add expr1 expr2 -> failure x
-- Sub expr1 expr2 -> failure x
-- Mul expr1 expr2 -> failure x
-- Div expr1 expr2 -> failure x
-- Neg expr -> failure x

labelsFromList :: LabelList -> String
labelsFromList (LabelList (Label s)) = s

labelFromExpr :: Expr -> String
labelFromExpr (Tensor (Label s)) = s

labelFromIdent :: Ident -> String
labelFromIdent (Ident s) = s

numsToInts :: [NumList] -> [Int]
numsToInts ns = map (fromInteger . numListToInteger) ns

numListToInteger :: NumList -> Integer
numListToInteger (NumList n) = n

tensorAppend :: TensorType -> State BookState ()
tensorAppend tensor = modify (\t -> t { bookTensors = tensor : bookTensors t })

funcAppend :: FunctionType -> State BookState ()
funcAppend f = modify (\t -> t { bookFuncs = f : bookFuncs t })

runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run

run :: String -> IO ()
run s = case parse s of
    Bad s    -> do putStrLn "\nParse              Failed...\n"
                   putStrLn s
                   exitFailure
    Ok  tree -> do putStrLn "\nParse Successful!"
                   showTree tree
                   putStrLn $ show $ execState (analyzeBook tree) (BookState [] [])
                   exitSuccess

parse :: String -> Err Book
parse s = pBook (tokens s)

showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
      putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> hGetContents stdin >>= run
    "-s":fs -> mapM_ runFile fs
    fs -> mapM_ runFile fs
