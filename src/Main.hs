module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )

import Frontend.LexTensor ( tokens )
import Frontend.ParTensor ( pBook )
import Frontend.PrintTensor
import Frontend.AbsTensor
import Frontend.ErrM
import Check ( analyzeBook )

import Control.Monad.State

import Core (
    BookState(..)
 )

runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run

run :: String -> IO ()
run s = case parse s of
    Bad s    -> do putStrLn "\nParse              Failed...\n"
                   putStrLn s
                   exitFailure
    Ok  tree -> do putStrLn "\nParse Successful!"
                   showTree tree
                   putStrLn $ show $ execStateT (analyzeBook tree) (BookState [] [])
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
