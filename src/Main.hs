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
import Data.Foldable
import Data.Maybe
import Data.List as L
import qualified Data.Text as T
import Data.Map as M
import Debug.Trace

import Control.Monad.State
import Control.Monad.Reader
import RenderCalc

import Eval
import XPerm

import Core (
    BookState(..),
    Calc,
    TensorType(..),
    FunctionType(..),
    OpType(..),
    tensorTypeFromCalc,
    emptyBook
 )

import CalcFromExpr (calcFromExpr)

lambdaPrompt = "λ> "
evalPrefix = "\t"

putColor :: Color -> String -> IO ()
putColor color msg = do
  setSGR [SetColor Foreground Vivid color]
  putStr msg
  setSGR [Reset]
  putStrLn ""

putErr :: String -> IO ()
putErr msg = putColor Red msg

putSuccess :: String -> IO ()
putSuccess msg = putColor Green msg

-- runFile :: FilePath -> IO ()
-- runFile f = putStrLn f >> readFile f >>= run

type ReplState = (BookState, [String])

bookstateCompletion :: BookState -> [String]
bookstateCompletion bs = tensors ++ funcs ++ ops
    where tensors = M.keys $ bookTensors bs
          funcs = L.map funcName $ bookFuncs bs
          ops = L.map opName $ bookOps bs

attemptCompletion :: BookState -> String -> Int -> Int -> IO (Maybe (String, [String]))
attemptCompletion bs s _ _ = do
    setAttemptedCompletionOver True
    return $
        let word = s
            candidates = L.filter (isPrefixOf word) (bookstateCompletion bs)
            longest = commonPrefixAll candidates
        in if length candidates > 0 then Just (longest, candidates) else Nothing

commonPrefix :: (Eq e) => [e] -> [e] -> [e]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x:xs) (y:ys)
  | x == y    = x : commonPrefix xs ys
  | otherwise = []

commonPrefixAll :: (Eq e) => [[e]] -> [e]
commonPrefixAll = L.foldl1 commonPrefix

showTensor :: TensorType -> IO ()
showTensor t = putStrLn $ tensorName t ++ " [" ++ (intercalate ", " (L.map show $ tensorIndices t)) ++ "]"

showTensors :: BookState -> IO ()
showTensors bs = mapM_ showTensor $ bookTensors bs

loadBook :: FilePath -> IO (BookState, String)
loadBook f = readFile f >>= return . (\s -> L.map ((++";") . T.unpack) $ L.filter (/= T.pack "") $ T.split (==';') $ T.pack s)
                        >>= (foldlM reduce emptyBook) >>= return . flip (,) ""
  where reduce bs s = do
          (bs', s') <- repl' bs s
          putStrLn s
          putSuccess s'
          return bs'

repl :: IO ()
repl = replRL emptyBook

replRL :: BookState -> IO ()
replRL bs = do
  setAttemptedCompletionFunction (Just $ attemptCompletion bs)
  l <- readline lambdaPrompt
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
          ':':'l':'o':'a':'d':rest -> do
            (bs', result) <- loadBook $ T.unpack (T.strip (T.pack rest))
            putSuccess result
            -- showTensors bs'
            replRL bs'
          stmt -> do
            (bs'', result) <- repl' bs (stmt ++ ";")
            putSuccess result
            replRL bs''

repl' :: BookState -> String -> IO (BookState, String)
repl' bs input = case parse input of
    Ok ast -> case analyzeBookWithState bs ast of
      (Bad typeErr) -> putErr typeErr >> return (bs, "")
      (Ok (book, bs')) -> do
        (bs'', result) <- evalBookConsole bs' book
        return (bs'', result)
    Bad parseErr -> putErr parseErr >> return (bs, "")

-- handleStmts :: BookState -> [Stmt] -> IO BookState
-- handleStmts bs [] = return bs
-- handleStmts bs (st:sts) = evalStatement bs st >>= flip handleStmts sts


-- run :: String -> IO ()
-- run s = case parse s of
--     Bad s    -> do putStrLn "\nParse              Failed...\n"
--                    putStrLn s
--                    exitFailure
--     Ok  tree -> do putStrLn "\nParse Successful!"
--                    showTree tree
--                    case analyzeBook tree of
--                     (Bad error) -> do
--                       putErr error
--                       exitFailure
--                     (Ok (book, bookState)) -> do
--                       let (Derivation stmts) = book
--                       let voids = map (\s -> niceexpr s >>= return . (\s -> runReader (calcFromExpr s) bookState)) stmts
--                       --let renderedVoids = map (liftM (renderCalc console id)) voids :: [Maybe String]
--                       --let indexLabels = map (\x -> "a" ++ show x) [0..]
--                       let renderState = ([], 0)
--                       let renderedVoids = map (liftM (\x->runReader (renderCalc console id x) renderState)) voids :: [Maybe String]
--                       mapM_ putSuccess (catMaybes renderedVoids)
--                       exitSuccess
--   where niceexpr (StmtVoid s) = Just s
--         niceexpr ss           = Nothing

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
-- main = putStrLn $ show $ testxperm [1,2,3,4]


-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     ["--help"] -> usage
--     [] -> hGetContents stdin >>= run
--     "-s":fs -> mapM_ runFile fs
--     fs -> mapM_ runFile