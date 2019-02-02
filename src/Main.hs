module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.Console.ANSI
import System.Console.Readline

import Frontend.LexTensor ( tokens )
import Frontend.ParTensor ( pBook )
import Frontend.PrintTensor
import Frontend.AbsTensor
import Frontend.ErrM
import Check ( analyzeBook, analyzeBookWithState )

import Data.Typeable
import Data.Maybe
import Data.List

import Control.Monad.State
import Control.Monad.Reader
import RenderCalc

import Core (
    BookState(..),
    Calc,
    TensorType(..),
    calcFromExpr,
    emptyBook
 )

putColor :: Color -> String -> IO ()
putColor color msg = do
  setSGR [SetColor Foreground Vivid color]
  putStrLn msg
  setSGR [Reset]

putErr :: String -> IO ()
putErr msg = putColor Red msg

putSuccess :: String -> IO ()
putSuccess msg = putColor Green msg

runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run

type ReplState = (BookState, [String])

evalExpr :: BookState -> Expr -> IO BookState
evalExpr bs expr = do
    let calc = runReader (calcFromExpr expr) bs
    putStrLn $ runReader (renderCalc console id calc) ([], 0)
    return $ appendCalc bs calc

evalStatement :: BookState -> Stmt -> IO BookState
evalStatement bs stmt  = case stmt of
    StmtAssign (Label var) expr -> putStr (var ++ " := ") >> evalExpr bs expr
    StmtVoid expr -> evalExpr bs expr
    StmtFuncDef name exprs stmts -> undefined
    StmtTensorDef ts ds -> (putSuccess $ concat $ map show ts) >> return bs

evalCommand :: String -> IO ()
evalCommand ":show tensors" = undefined
evalCommand ":show variables" = undefined
evalCommand ":show functions" = undefined
evalCommand ":show groups <tensor>" = undefined
evalCommand ":show all groups" = undefined
evalCommand ":show all groups <name>" = undefined

showTensor :: TensorType -> IO ()
showTensor t = putStrLn $ tensorName t ++ " [" ++ (intercalate ", " (map show $ tensorIndices t)) ++ "]"

showTensors :: BookState -> IO ()
showTensors bs = mapM_ showTensor $ bookTensors bs

repl :: IO ()
--repl = replB emptyBook >> return ()
repl = replRL emptyBook

replRL :: BookState -> IO ()
replRL bs = do
  l <- readline "λ> "
  case l of
    Nothing -> return ()
    Just "exit" -> return ()
    Just "" -> replRL bs
    Just line -> do
        addHistory line
        case line of
          ":show tensors" -> showTensors bs >> replRL bs
          ":show variables" -> undefined
          ":show functions" -> undefined
          ":show groups <tensor>" -> undefined
          ":show all groups" -> undefined
          ":show all groups <name>" -> undefined
          stmt -> repl' bs stmt >>= replRL


replB :: BookState -> IO BookState
-- replB bs = replB =<< repl' bs =<< getContents
replB bs = getLine >>= repl' bs >>= replB
-- >>=

  -- do
  -- lol <- getContents
  -- lol' <- repl' bs lol
  -- replB lol'

repl' :: BookState -> String -> IO BookState
repl' bs input = case parse input of
    Ok ast -> case analyzeBookWithState bs ast of
      (Bad typeErr) -> putErr typeErr >> return bs
      (Ok (book, bs')) -> do
        let (Derivation sts) = book
        handleStmts bs' sts
    Bad parseErr -> putErr parseErr >> return bs

handleStmts :: BookState -> [Stmt] -> IO BookState
handleStmts bs [] = return bs
handleStmts bs (st:sts) = evalStatement bs st >>= flip handleStmts sts

-- handleStmt :: 

appendCalc :: BookState -> Calc -> BookState
appendCalc bs c = bs { bookCalcs = c : bookCalcs bs }

run :: String -> IO ()
run s = case parse s of
    Bad s    -> do putStrLn "\nParse              Failed...\n"
                   putStrLn s
                   exitFailure
    Ok  tree -> do putStrLn "\nParse Successful!"
                   showTree tree
                   case analyzeBook tree of
                    (Bad error) -> do
                      putErr error
                      exitFailure
                    (Ok (book, bookState)) -> do
                      let (Derivation stmts) = book
                      let voids = map (\s -> niceexpr s >>= return . (\s -> runReader (calcFromExpr s) bookState)) stmts
                      --let renderedVoids = map (liftM (renderCalc console id)) voids :: [Maybe String]
                      --let indexLabels = map (\x -> "a" ++ show x) [0..]
                      let renderState = ([], 0)
                      let renderedVoids = map (liftM (\x->runReader (renderCalc console id x) renderState)) voids :: [Maybe String]
                      mapM_ putSuccess (catMaybes renderedVoids)
                      exitSuccess
  where niceexpr (StmtVoid s) = Just s
        niceexpr ss           = Nothing

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
main = repl


-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     ["--help"] -> usage
--     [] -> hGetContents stdin >>= run
--     "-s":fs -> mapM_ runFile fs
--     fs -> mapM_ runFile fs
