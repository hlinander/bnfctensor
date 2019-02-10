module Check where

import Frontend.PrintTensor
import Frontend.AbsTensor
import Frontend.ErrM

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.List

import Core (
    BookState(..),
    GroupType(..),
    IndexType(..),
    FunctionType(..),
    TensorType(..),
    lookupTensor,
    emptyBook
 )

import Tensor ( freeIndices,
                usedIndices )

analyzeBook :: Book -> Err (Book, BookState)
analyzeBook book = runStateT (analyzeBook' book) emptyBook

analyzeBookWithState :: BookState -> Book -> Err (Book, BookState)
analyzeBookWithState = flip (runStateT . analyzeBook')

analyzeBook' :: Book -> StateT BookState Err Book
analyzeBook' (Derivation ss) = mapM analyzeStmt ss >>= return . Derivation

analyzeStmt :: Stmt -> StateT BookState Err Stmt
analyzeStmt stmt = case stmt of
    StmtTensorDef labels tensordef -> analyzeTensorDef labels tensordef >> return stmt
    StmtFuncDef ident exprs stmts -> funcAppend (FunctionType (labelFromIdent ident) (length exprs)) >> return stmt
    StmtAssign label expr -> runReaderT (analyzeExpr expr) ([] :: [Index]) >> return stmt
    StmtVoid expr -> runReaderT (analyzeExpr expr) ([] :: [Index]) >> return stmt
    _ -> return stmt

analyzeTensorDef :: [LabelList] -> [TensorDef] -> StateT BookState Err ()
analyzeTensorDef lls def = mapM_ maybeAppend (map tensor labels)
    where labels = map labelsFromList lls
          tensor label = TensorType label (analyzeIndices def)
          maybeAppend l = do
            defined <- tensorDefined l
            if defined then fail $ "Tensor already defined: " -- ++ show $ lookupTensor l
            else tensorAppend l

analyzeIndex :: TensorDef -> [IndexType]
analyzeIndex (TensorDef indices (GroupDef (Label gl) nums)) = map indexType indices
    where group = GroupType gl $ numsToInts nums
          indexType (IndexGroup (Label il) idim) = IndexType (fromInteger idim) group il

analyzeIndices :: [TensorDef] -> [IndexType]
analyzeIndices defs = concatMap analyzeIndex defs

analyzeExpr :: Expr -> ReaderT [Index] (StateT BookState Err) Expr
analyzeExpr expr = do
    currentIndices <- ask
    let indices = usedIndices expr
    let overlap = (intersect currentIndices indices)
    when (not $ nub indices == indices) $ fail $ "Index label collision in [" ++ printTree expr ++ "]\n"
    do
        let exprMsg  = "Index label collisions in [" ++ printTree expr ++ "]\n"
        let indexMsg = "for indices " ++ printTree overlap ++ "\n"
        let reserved = "(reserved indices " ++ printTree currentIndices ++ ")"
        when (length overlap > 0) $ fail $ exprMsg ++ indexMsg ++ reserved
    case expr of
        Add e1 e2 -> do
            a1 <- analyzeExpr e1
            a2 <- analyzeExpr e2
            checkPlus a1 a2
        Sub e1 e2 -> do
            a1 <- analyzeExpr e1
            a2 <- analyzeExpr e2
            checkPlus a1 a2
        Mul e1 e2 -> do
            _ <- local (union $ usedIndices e2) $ analyzeExpr e1
            _ <- local (union $ usedIndices e1) $ analyzeExpr e2
            return expr
        Tensor label indices -> checkTensorDecl label indices >> return expr
        _ -> return expr
        where checkPlus e1 e2 = let free1 = freeIndices e1
                                    free2 = freeIndices e2
                                in case sort free1 == sort free2 of
                                    True -> return expr
                                    False -> fail $ "Free indices in "
                                        ++ "(" ++ (printTree e1) ++ ")"
                                        ++ " + "
                                        ++ "(" ++ (printTree e2) ++ ")"
                                        ++ " does not match, i.e. "
                                        ++ "{" ++ printNonEmptyIndices (freeIndices e1) ++ "}"
                                        ++ " != "
                                        ++ "{" ++ printNonEmptyIndices (freeIndices e2) ++ "}"

printNonEmptyIndices :: [Index] -> String
printNonEmptyIndices [] = ""
printNonEmptyIndices expr = printTree expr


checkTensorDecl :: Label -> [Index] -> ReaderT [Index] (StateT BookState Err) ()
checkTensorDecl (Label s) indices = do
    tensorType <- findDeclTensor s
    case tensorType of
        [] -> fail $ "Tensor " ++ s ++ " not declared"
        ((TensorType name defIndices) : []) | length defIndices == length indices -> return ()
        ((TensorType name defIndices) : []) -> fail $ "Tensor " ++ s ++ " used with wrong rank"
        _ -> fail $ "Tensor " ++ s ++ " declared multiple times"

findDeclTensor :: String -> ReaderT [Index] (StateT BookState Err) [TensorType]
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

tensorDefined :: Monad m => TensorType -> StateT BookState m Bool
tensorDefined l = do
    bs <- get
    return $ any (\t -> tensorName t == tensorName l) $ bookTensors bs