module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.Console.ANSI

import Frontend.LexTensor ( tokens )
import Frontend.ParTensor ( pBook )
import Frontend.PrintTensor
import Frontend.AbsTensor
import Frontend.ErrM
import Check ( analyzeBook )

import Data.Typeable
import Data.Maybe

import Control.Monad.State
import Control.Monad.Reader
import RenderCalc

import Core (
    BookState(..),
    Calc,
    calcFromExpr
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

run :: String -> IO ()
run s = case parse s of
    Bad s    -> do putStrLn "\nParse              Failed...\n"
                   putStrLn s
                   exitFailure
    Ok  tree -> do putStrLn "\nParse Successful!"
                   showTree tree
                   case runStateT (analyzeBook tree) (BookState [] []) of
                    (Bad error) -> do
                      putErr error
                      exitFailure
                    (Ok (book, bookState)) -> do
                      let (Derivation stmts) = book
                      let voids = map (\s -> niceexpr s >>= return . (\s -> runReader (calcFromExpr s) bookState)) stmts
                      let renderedVoids = map (liftM (renderCalc console id)) voids :: [Maybe String]
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
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> hGetContents stdin >>= run
    "-s":fs -> mapM_ runFile fs
    fs -> mapM_ runFile fs
