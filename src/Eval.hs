{-# LANGUAGE FlexibleContexts #-}

module Eval (
    evalBookConsole,
    evalBookMathML
) where

import Control.Monad.Reader
import qualified Data.Map as M

import Core
import RenderCalc
import Frontend.AbsTensor
import Frontend.PrintTensor
import CalcFromExpr (calcFromExpr)

import Prelude hiding ( printCalc )

evalBookConsole :: BookState -> Book -> IO (BookState, String)
evalBookConsole bs book = evalBook bs book Console

evalBookMathML :: BookState -> Book -> IO (BookState, String)
evalBookMathML bs book = evalBook bs book MathML

evalBook bs book = runReaderT (handleStmts bs stmts)
    where (Derivation stmts) = book

evalExpr :: BookState -> Expr -> (ReaderT PrintMode IO) (BookState, String)
evalExpr bs expr = do
    mode <- ask
    case calcFromExpr expr bs of
        Left err -> return (bs, err)
        Right calc -> return (bs, printCalc mode calc)

-- substitute : replace with new tensor def variable
-- expand variable : replace with contents of variable


-- evalAssignExpr :: BookState -> String -> Expr -> IO (BookState, String)
evalAssignExpr bs var expr = do
        mode <- ask
        (bs', render) <- evalExpr bs expr
        return (bs', printCalc mode ("[" ++ var ++ "]: ") ++ render)

--evalStatement :: BookState -> Stmt -> IO (BookState, String)
evalStatement bs stmt  = do
    mode <- ask
    case stmt of
        StmtAssign (Label var) expr -> evalAssignExpr bs var expr
        StmtVoid expr -> evalAssignExpr bs (currentAnonymous bs) expr
        StmtFuncDef name exprs stmts -> undefined
        std@(StmtTensorDef ts ds) -> return (bs, printCalc mode $ printTree std)
        sto@(StmtOpDef os ds) -> return (bs, printCalc mode $ printTree sto)
        StmtRowDoc doc -> return (bs, printCalc mode $ doc)

--handleStmts :: BookState -> [Stmt] -> (ReaderT RenderTarget IO) (BookState, String)
handleStmts bs [] = return (bs, "")
handleStmts bs (st:sts) = do
    (bs', out) <- evalStatement bs st
    (bs'', out') <- handleStmts bs' sts
    return (bs'', out ++ "\n" ++ out')

--bookstateCompletion :: BookState -> [String]
bookstateCompletion bs = tensors ++ funcs ++ ops
    where tensors = M.keys $ bookTensors bs
          funcs = map funcName $ bookFuncs bs
          ops = map opName $ bookOps bs