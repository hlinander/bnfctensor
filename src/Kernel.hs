module Main where

import qualified Data.Text as T

import System.Environment (getArgs)

import Control.Concurrent
import Control.Monad.Reader

import Data.List

import qualified Data.Map as M

import IHaskell.IPython.EasyKernel (easyKernel, installKernelspec, KernelConfig(..))
import IHaskell.IPython.Types

import Core
import Check (analyzeBookWithState)
import Frontend.AbsTensor
import Frontend.LexTensor (tokens)
import Frontend.ParTensor (pBook)
import qualified Frontend.ErrM as E
import RenderCalc


languageConfig :: LanguageInfo
languageConfig = LanguageInfo
    { languageName = "tensorkernel"
    , languageVersion = "1.0.0"
    , languageFileExtension = ".book"
    , languageCodeMirrorMode = "null"
    , languagePygmentsLexer = "Text"
    }

languageKernelspec :: KernelSpec
languageKernelspec = KernelSpec
    { kernelDisplayName = "Tensor"
    , kernelLanguage = "tensor"
    , kernelCommand = ["tensorkernel", "kernel", "{connection_file}"]
    }

displayString :: String -> [DisplayData]
displayString str = [DisplayData MimeHtml (T.pack str)]

parse :: String -> E.Err Book
parse s = pBook (tokens s)

appendCalc :: BookState -> String -> Calc -> BookState
appendCalc bs s c = bs
    {   bookCalcs = M.insert s c $ bookCalcs bs
    ,   bookTensors = (tensorTypeFromCalc s c) : (bookTensors bs) }

evalExpr :: BookState -> Expr -> IO (BookState, String)
evalExpr bs expr = do
    let (calc, idx) = runReader (calcFromExpr expr) bs
    return (bs, "<math>" ++ renderCalc calc mathML ++ "</math><br>")

-- substitute : replace with new tensor def variable
-- expand variable : replace with contents of variable

evalStatement :: BookState -> Stmt -> IO (BookState, String)
evalStatement bs stmt  = case stmt of
    StmtAssign (Label var) expr -> do
        let (calc, idx) = runReader (calcFromExpr expr) bs
        let bs' = appendCalc bs var calc
        evalExpr bs' expr
    StmtVoid expr -> evalExpr bs expr
    StmtFuncDef name exprs stmts -> undefined
    StmtTensorDef ts ds -> return (bs, concat $ map show ts)
    StmtOpDef os ds -> return (bs, concat $ map show os)

handleStmts :: BookState -> [Stmt] -> IO (BookState, String)
handleStmts bs [] = return (bs, "")
handleStmts bs (st:sts) = do
    (bs', out) <- evalStatement bs st
    (bs'', out') <- handleStmts bs' sts
    return (bs'', out ++ out')

runKernel :: MVar BookState -> T.Text -> IO () -> (String -> IO ()) -> IO (String, ExecuteReplyStatus, String)
runKernel mbs input _ _ = case parse $ T.unpack input of
    E.Ok ast -> do
        bs <- takeMVar mbs
        case analyzeBookWithState bs ast of
            (E.Bad typeErr) -> putMVar mbs bs >> return (typeErr, IHaskell.IPython.Types.Err, "")
            (E.Ok (book, bs')) -> do
                let (Derivation sts) = book
                (bs'', render) <- handleStmts bs' sts
                putMVar mbs bs''
                return (render, IHaskell.IPython.Types.Ok, show ast ++ "\n" ++ show bs'')
    E.Bad parseErr -> return (parseErr, IHaskell.IPython.Types.Err, "")

bookstateCompletion :: BookState -> [String]
bookstateCompletion bs = tensors ++ funcs ++ ops
    where tensors = map tensorName $ bookTensors bs
          funcs = map funcName $ bookFuncs bs
          ops = map opName $ bookOps bs


languageCompletion :: MVar BookState -> T.Text -> Int -> IO (T.Text, [T.Text])
languageCompletion mbs code pos = do
    bs <- takeMVar mbs
    putMVar mbs bs
    return $
        let (before, _) = T.splitAt pos code
            word = last $ T.words $ T.map replace before
            candidates = filter (isPrefixOf $ T.unpack word) (bookstateCompletion bs)
        in (word, map T.pack candidates)

  where
    replace :: Char -> Char
    replace '(' = ' '
    replace ')' = ' '
    replace ',' = ' '
    replace x = x

simpleConfig :: MVar BookState -> KernelConfig IO String String
simpleConfig mbs = KernelConfig
    { kernelLanguageInfo = languageConfig
    , writeKernelspec = const $ return languageKernelspec
    , displayOutput = displayString
    , displayResult = displayString
    , completion = languageCompletion mbs
    -- , inspectInfo = languageInspect
    , run = runKernel mbs
    , debug = False
    , kernelBanner = "Tensor Manipulation Assistant"
    , kernelProtocolVersion = "5.0"
    , kernelImplName = "tensorkernel"
    , kernelImplVersion = "0.0"
    }

main :: IO ()
main = getArgs >>= \args -> case args of
    ["run", profileFile] -> do
        var <- newMVar emptyBook
        easyKernel profileFile (simpleConfig var)
    ["install"] -> do
        putStrLn "Installing kernelspec..."
        var <- newMVar emptyBook
        installKernelspec (simpleConfig var) False Nothing
    _ -> do
        putStrLn "Usage:"
        putStrLn "tensorkernel install      -- set up the kernelspec"
        putStrLn "tensorkernel run FILE  -- run a kernel with FILE for communication with the frontend"