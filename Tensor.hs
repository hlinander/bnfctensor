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

type ParseFun a = [Token] -> Err a

type Result = Err Book

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

data TensorType = TensorType {
    tensorName :: String,
    tensorDim :: Int
} deriving Show

data FunctionType = FunctionType {
    funcName :: String,
    funcArity :: Int
} deriving Show

data BookState = D {
    bookTensors :: [TensorType],
    bookFuncs :: [FunctionType]
} deriving Show

analyzeBook :: Book -> State BookState ()
analyzeBook (Derivation ss) = mapM_ analyzeStmt ss

analyzeStmt :: Stmt -> State BookState ()
analyzeStmt stmt = case stmt of
    TensorDef exprs n -> mapM_ (\expr -> dimAppend $ d expr n) exprs
    FuncDef ident exprs stmts -> funcAppend $ FunctionType (labelFromIdent ident) (length exprs)
    Assign label expr -> analyzeExpr expr
    Void expr -> analyzeExpr expr
    _ -> return ()
    where d x n = TensorType (labelFromExpr x) (fromInteger n)

analyzeExpr :: Expr -> State BookState ()
analyzeExpr expr = case expr of
    -- Indexed expr indices -> undefined
    Tensor label -> checkTensorDecl label
    _ -> return ()

checkTensorDecl :: Label -> State BookState ()
checkTensorDecl (Label s) = do
    tensorType <- findDeclTensor s
    case tensorType of
        [] -> haxxorPrint ("Tensor '" ++ s ++ "' not declared") >> return ()
        ((TensorType name dim) : []) -> haxxorPrint ("Tensor '" ++ s ++ "' not declared") >> return ()
        (t : ts) -> haxxorPrint ("Tensor '" ++ s ++ "' declared multiple times") >> return ()
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
        False -> return $ haxxorPrint $ "Tensor '" ++ s ++ "' not declared"

findDeclTensor :: String -> State BookState [TensorType]
findDeclTensor s = do
    bookState <- get
    return $ filter (\t -> tensorName t == s) (bookTensors bookState)

haxxorPrint :: String -> ()
haxxorPrint s = unsafePerformIO $ putStrLn s

-- Func label exprs -> failure x
-- Add expr1 expr2 -> failure x
-- Sub expr1 expr2 -> failure x
-- Mul expr1 expr2 -> failure x
-- Div expr1 expr2 -> failure x
-- Neg expr -> failure x

labelFromExpr :: Expr -> String
labelFromExpr (Tensor (Label s)) = s

labelFromIdent :: Ident -> String
labelFromIdent (Ident s) = s

dimAppend :: TensorType -> State BookState ()
dimAppend tdim = modify (\t -> t { bookTensors = tdim : bookTensors t })

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
                   putStrLn $ show $ execState (analyzeBook tree) (D [] [])
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
