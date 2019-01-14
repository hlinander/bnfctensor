module Check where

import Frontend.PrintTensor
import Frontend.AbsTensor
import Frontend.ErrM

import Control.Monad.State
import Data.List

import Core (
    BookState(..),
    GroupType(..),
    IndexType(..),
    FunctionType(..),
    TensorType(..)
 )

import Tensor ( freeIndices )

analyzeBook :: Book -> StateT BookState Err Book
analyzeBook (Derivation ss) = mapM analyzeStmt ss >>= return . Derivation

analyzeStmt :: Stmt -> StateT BookState Err Stmt
analyzeStmt stmt = case stmt of
    StmtTensorDef labels tensordef -> analyzeTensorDef labels tensordef >> return stmt
    StmtFuncDef ident exprs stmts -> funcAppend (FunctionType (labelFromIdent ident) (length exprs)) >> return stmt
    StmtAssign label expr -> analyzeExpr expr >> return stmt
    StmtVoid expr -> analyzeExpr expr >> return stmt
    _ -> return stmt

analyzeTensorDef :: [LabelList] -> [TensorDef] -> StateT BookState Err ()
analyzeTensorDef lls def = mapM_ tensorAppend (map tensor labels)
    where labels = map labelsFromList lls
          tensor label = TensorType label (analyzeIndices def)

analyzeIndex :: TensorDef -> [IndexType]
analyzeIndex (TensorDef indices (GroupDef (Label gl) nums)) = map indexType indices
    where group = GroupType gl $ numsToInts nums
          indexType (IndexGroup (Label il) idim) = IndexType (fromInteger idim) group il

analyzeIndices :: [TensorDef] -> [IndexType]
analyzeIndices defs = concatMap analyzeIndex defs

analyzeExpr :: Expr -> StateT BookState Err Expr
analyzeExpr expr = case expr of
    -- Indexed expr indices -> undefined
    Add e1 e2 -> case sort (freeIndices e1) == sort (freeIndices e2) of
        True -> return expr
        False -> fail $ "Free indices in (" ++ (printTree e1) ++ ") + (" 
            ++ printTree e2 ++ ") does not match"
    Tensor label indices -> checkTensorDecl label >> return expr
    _ -> return expr

checkTensorDecl :: Label -> StateT BookState Err ()
checkTensorDecl (Label s) = do
    tensorType <- findDeclTensor s
    case tensorType of
        -- [] -> haxxorPrint ("Tensor '" ++ s ++ "' not declared") >> return ()
        ((TensorType name dim) : []) -> return ()
        _ -> fail "FUCK KNOWS"
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

findDeclTensor :: String -> StateT BookState Err [TensorType]
findDeclTensor s = do
    bookState <- get
    return $ filter (\t -> tensorName t == s) (bookTensors bookState)

labelsFromList :: LabelList -> String
labelsFromList (LabelList (Label s)) = s

labelFromExpr :: Expr -> String
labelFromExpr (Tensor (Label s) _) = s

labelFromIdent :: Ident -> String
labelFromIdent (Ident s) = s

numsToInts :: [NumList] -> [Int]
numsToInts ns = map (fromInteger . numListToInteger) ns

numListToInteger :: NumList -> Integer
numListToInteger (NumList n) = n

tensorAppend :: Monad m => TensorType -> StateT BookState m ()
tensorAppend tensor = modify (\t -> t { bookTensors = tensor : bookTensors t })

funcAppend :: Monad m => FunctionType -> StateT BookState m ()
funcAppend f = modify (\t -> t { bookFuncs = f : bookFuncs t })