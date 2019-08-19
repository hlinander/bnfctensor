module Main where

import qualified Data.Text as T

import System.Environment (getArgs)

import Control.Concurrent
import Control.Monad.Reader

import Data.List

import qualified Data.Map as M

import IHaskell.IPython.EasyKernel
  ( KernelConfig(..)
  , easyKernel
  , installKernelspec
  )
import IHaskell.IPython.Types

import Check (analyzeBookWithState)
import Core
import Eval
import Frontend.AbsTensor
import qualified Frontend.ErrM as E
import Frontend.LexTensor (tokens)
import Frontend.ParTensor (pBook)
import RenderCalc

languageConfig :: LanguageInfo
languageConfig =
  LanguageInfo
    { languageName = "tensorkernel"
    , languageVersion = "1.0.0"
    , languageFileExtension = ".book"
    , languageCodeMirrorMode = "null"
    , languagePygmentsLexer = "Text"
    }

languageKernelspec :: KernelSpec
languageKernelspec =
  KernelSpec
    { kernelDisplayName = "Tensor"
    , kernelLanguage = "tensor"
    , kernelCommand = ["tensorkernel", "kernel", "{connection_file}"]
    }

displayString :: String -> [DisplayData]
displayString str = [DisplayData MimeHtml (T.pack str)]

parse :: String -> E.Err Book
parse s = pBook (tokens s)

appendCalc :: BookState -> String -> Calc -> BookState
appendCalc bs s c =
  bs
    { bookCalcs = M.insert s c $ bookCalcs bs
    , bookTensors = M.insert (tensorName tensor) tensor (bookTensors bs)
    }
  where
    tensor = (tensorTypeFromCalc s c)

runKernel ::
     MVar BookState
  -> T.Text
  -> IO ()
  -> (String -> IO ())
  -> IO (String, ExecuteReplyStatus, String)
runKernel mbs input _ _ =
  case parse $ T.unpack input of
    E.Ok ast -> do
      bs <- takeMVar mbs
      case analyzeBookWithState bs ast of
        (E.Bad typeErr) ->
          putMVar mbs bs >> return (typeErr, IHaskell.IPython.Types.Err, "")
        (E.Ok (book, bs')) -> do
          (bs'', render) <- evalBookMathML bs' book
          putMVar mbs bs''
          return (render, IHaskell.IPython.Types.Ok, "") -- show ast ++ "\n" ++ show bs'')
    E.Bad parseErr -> return (parseErr, IHaskell.IPython.Types.Err, "")

bookstateCompletion :: BookState -> [String]
bookstateCompletion bs = tensors ++ funcs ++ ops
  where
    tensors = M.keys $ bookTensors bs
    funcs = map funcName $ bookFuncs bs
    ops = map opName $ bookOps bs

languageCompletion :: MVar BookState -> T.Text -> Int -> IO (T.Text, [T.Text])
languageCompletion mbs code pos = do
  bs <- takeMVar mbs
  putMVar mbs bs
  return $
    let (before, _) = T.splitAt pos code
        word = last $ T.words $ T.map replace before
        candidates =
          filter (isPrefixOf $ T.unpack word) (bookstateCompletion bs)
     in (word, map T.pack candidates)
  where
    replace :: Char -> Char
    replace '(' = ' '
    replace ')' = ' '
    replace ',' = ' '
    replace x = x

peekMVar :: MVar a -> IO a
peekMVar v = takeMVar v >>= (\x -> putMVar v x >> return x)

languageInspect :: MVar BookState -> T.Text -> Int -> IO (Maybe [DisplayData])
languageInspect mbs code pos = do
  a <- peekMVar mbs
  return $ Just $ displayString "HELLO INSPECT"

simpleConfig :: MVar BookState -> KernelConfig IO String String
simpleConfig mbs =
  KernelConfig
    { kernelLanguageInfo = languageConfig
    , writeKernelspec = const $ return languageKernelspec
    , displayOutput = displayString
    , displayResult = displayString
    , completion = languageCompletion mbs
    , inspectInfo = languageInspect mbs
    , run = runKernel mbs
    , debug = False
    , kernelBanner = "Tensor Manipulation Assistant"
    , kernelProtocolVersion = "5.0"
    , kernelImplName = "tensorkernel"
    , kernelImplVersion = "0.0"
    }

main :: IO ()
main =
  getArgs >>= \args ->
    case args of
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
        putStrLn
          "tensorkernel run FILE  -- run a kernel with FILE for communication with the frontend"
