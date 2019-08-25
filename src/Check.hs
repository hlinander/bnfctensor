module Check where

import Frontend.PrintTensor
import Frontend.AbsTensor
import Frontend.ErrM

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Data.List
import Data.Maybe
import qualified Data.Map as M

import Math.Combinat.Permutations

import Core (
    BookState(..),
    GroupType(..),
    IndexType(..),
    ReprType(..),
    FunctionType(..),
    TensorType(..),
    SymmetryType(..),
    OpType(..),
    Calc,
    emptyBook,
    tensorTypeFromCalc,
    lookupTensor,
    nextAnonymous
 )

import CalcFromExpr (calcFromExpr)

import Tensor ( freeIndices,
                usedIndices )

analyzeBook :: Book -> Err (Book, BookState)
analyzeBook book = runStateT (analyzeBook' book) emptyBook

analyzeBookWithState :: BookState -> Book -> Err (Book, BookState)
analyzeBookWithState = flip (runStateT . analyzeBook')

analyzeBook' :: Book -> StateT BookState Err Book
analyzeBook' (Derivation ss) = Derivation <$> mapM analyzeStmt ss

analyzeStmt :: Stmt -> StateT BookState Err Stmt
analyzeStmt stmt = case stmt of
    StmtTensorDef labels tensordef -> analyzeTensorDef labels tensordef >> return stmt
    StmtOpDef labels opdef -> analyzeOpDef labels opdef >> return stmt
    StmtFuncDef ident exprs _ -> funcAppend (FunctionType (labelFromIdent ident) (length exprs)) >> return stmt
    StmtSymmetry sym -> analyzeSymDef sym >> return stmt
    StmtAssign (Label l) expr -> do
        _ <- runReaderT (analyzeExpr expr) ([] :: [Index])
        bs <- get
        case calcFromExpr expr bs of
            Left err -> fail err
            Right calc -> calcAppend l calc >> return stmt
    StmtVoid expr -> do
        _ <- runReaderT (analyzeExpr expr) ([] :: [Index])
        bs <- get
        -- let (calc, _) = runReaderT (calcFromExpr exr) bs
        -- return stmt
        case calcFromExpr expr bs of
            Left err -> fail err
            Right calc -> anonymousAppend calc >> return stmt
    _ -> return stmt

-- analyzeTensorDef :: [LabelList] -> [TensorDef] -> StateT BookState Err ()
-- analyzeTensorDef lls def = mapM_ (maybeAppend . tensor) labels
--     where labels = map labelsFromList lls
--           tensor label = TensorType label (analyzeIndices def)
--           maybeAppend l = do
--             defined <- tensorDefined l
--             if defined then fail "Tensor already defined: " -- ++ show $ lookupTensor l
--             else tensorAppend l

analyzeTensorDef :: [LabelList] -> [TensorDef] -> StateT BookState Err ()
analyzeTensorDef lls def = do
    indices <- analyzeIndices def
    let labels = map labelsFromList lls
        tensor label = TensorType label indices []
        maybeAppend l = do
          defined <- tensorDefined l
          if defined then fail "Tensor already defined: " -- ++ show $ lookupTensor l
          else tensorAppend l
    mapM_ (maybeAppend . tensor) labels

analyzeSymDef :: Sym -> StateT BookState Err ()
analyzeSymDef = symmetryAppend

analyzeOpDef :: [LabelList] -> [TensorDef] -> StateT BookState Err ()
analyzeOpDef lls def = do
    indices <- analyzeIndices def
    let labels = map labelsFromList lls
        op label = OpType label indices
        maybeAppend l = do
          defined <- opDefined l
          if defined then fail "Op already defined: " -- ++ show $ lookupTensor l
          else opAppend l
    mapM_ (maybeAppend . op) labels

analyzeIndex :: TensorDef -> (StateT BookState Err) [IndexType]
analyzeIndex (ScalarDef) = return []
analyzeIndex (TensorDef indices (GroupDef (Label gl) nums)) = do
    let group = GroupType gl $ numsToInts nums
        indexType (IndexGroup (Label il) idim) = ReprType (fromInteger idim) group
        reprs = map indexType indices
        st = symmetrize False 2
    _ <- mapM (\r -> metricAppend r (TensorType "g" [r, r] st)) reprs
    mapM (return . indexType) indices

analyzeIndices :: [TensorDef] -> (StateT BookState Err) [IndexType]
analyzeIndices tds = liftM concat $ mapM analyzeIndex tds

analyzeExpr :: Expr -> ReaderT [Index] (StateT BookState Err) Expr
analyzeExpr expr = do
    currentIndices <- ask
    let indices = usedIndices expr
    let overlap = currentIndices `intersect` indices
    unless (nub indices == indices) $ fail $ "Index label collision in [" ++ printTree expr ++ "]\n"
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
        Op label indices e -> do
            _ <- local (union $ indices) $ analyzeExpr e
            checkOpDecl label indices >> return expr
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
        Nothing -> fail $ "Tensor " ++ s ++ " not declared"
        Just (TensorType _ defIndices _) | length defIndices == length indices -> return ()
        Just (TensorType _ defIndices _) -> fail $ "Tensor " ++ s ++ " used with wrong rank, expected " ++ (show defIndices)
        _ -> fail $ "Tensor " ++ s ++ " declared multiple times"

checkOpDecl :: Label -> [Index] -> ReaderT [Index] (StateT BookState Err) ()
checkOpDecl (Label s) indices = do
    opType <- findDeclOp s
    case opType of
        [] -> fail $ "Op " ++ s ++ " not declared"
        ((OpType _ defIndices) : []) | length defIndices == length indices -> return ()
        ((OpType _ _) : []) -> fail $ "Op " ++ s ++ " used with wrong rank"
        _ -> fail $ "Op " ++ s ++ " declared multiple times"

findDeclTensor :: String -> ReaderT [Index] (StateT BookState Err) (Maybe TensorType)
findDeclTensor s = do
    bookState <- get
    return $ M.lookup s (bookTensors bookState)

findDeclOp :: String -> ReaderT [Index] (StateT BookState Err) [OpType]
findDeclOp s = do
    bookState <- get
    return $ filter (\t -> opName t == s) (bookOps bookState)


labelsFromSymmetry :: Sym -> [String]
labelsFromSymmetry sym = case sym of
    Symmetric lls -> map labelsFromList lls
    AntiSymmetric lls -> map labelsFromList lls
    Equality lls _ -> map labelsFromList lls

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

anonymousAppend :: Monad m => Calc -> StateT BookState m ()
anonymousAppend c = get >>= flip calcAppend c . nextAnonymous

calcAppend :: Monad m => String -> Calc -> StateT BookState m ()
calcAppend s c = modify(\t -> t {
    bookCalcs = M.insert s c $ bookCalcs t,
    bookTensors = M.insert s (tensorTypeFromCalc s c) $ (bookTensors t)
})

metricAppend :: Monad m => ReprType -> TensorType -> StateT BookState m ()
metricAppend r t = modify(\bs -> bs {
    bookTensors = case lookupTensor (tensorName t) bs of
        Just _ -> bookTensors bs
        Nothing -> M.insert (tensorName t) t $ bookTensors bs,
    bookMetrics = M.insert r t $ bookMetrics bs
})

tensorAppend :: Monad m => TensorType -> StateT BookState m ()
tensorAppend tensor = modify (\t -> t { bookTensors = M.insert (tensorName tensor) tensor $ bookTensors t })

symmetryAppend :: Monad m => Sym -> StateT BookState m ()
symmetryAppend sym = do
    bs <- get
    let tt l = fromJust $ M.lookup l (bookTensors bs) 
        nIndices = length . tensorIndices . tt
        unpackLabels = map labelsFromList
        attachSymmetry st t = t { tensorSymmetries = st }
        makeAndInsertTypeOnTensor tensors l st = M.insert l (attachSymmetry st (tt l)) tensors
        updateTensors tensors anti lls = 
            foldr (\l tensors -> makeAndInsertTypeOnTensor tensors l 
            (symmetrize anti $ nIndices l)) tensors lls
    case sym of -- [1 2 3 4] -> [[1, 2], [2, 3], [3, 4], [4, 1]]
        Symmetric lls -> modify (\bs -> bs { bookTensors = updateTensors (bookTensors bs) False (unpackLabels lls) })
        AntiSymmetric lls -> modify (\bs -> bs { bookTensors = updateTensors (bookTensors bs) True (unpackLabels lls) })

-- bool True => antisymmetric False => symmetric
symmetrize :: Bool -> Int -> [SymmetryType]
symmetrize anti n = map (\idxSym -> SymmetryType 
            { indexSymmetry = idxSym
            ,   signSymmetry = toPermutation $ if anti then [2,1] else [1,2]
            }) idxPerms 
    where 
          swapPair = transposition n
          pairs n = map (\i -> (i, i + 1)) [1..(n-1)]
          idxPerms = map swapPair $ pairs n 


opAppend :: Monad m => OpType -> StateT BookState m ()
opAppend op = modify (\t -> t { bookOps = op : bookOps t })

funcAppend :: Monad m => FunctionType -> StateT BookState m ()
funcAppend f = modify (\t -> t { bookFuncs = f : bookFuncs t })

tensorDefined :: Monad m => TensorType -> StateT BookState m Bool
tensorDefined l = do
    bs <- get
    return $ M.member (tensorName l) $ bookTensors bs

symmetryDefined :: Monad m => TensorType -> StateT BookState m Bool
symmetryDefined t = do
    bs <- get
    let tensorType = M.lookup (tensorName t) $ bookTensors bs
    case tensorType of
        Just ts -> return $ [] /= (tensorSymmetries ts)
        Nothing -> return False

opDefined :: Monad m => OpType -> StateT BookState m Bool
opDefined l = do
    bs <- get
    return $ any (\t -> opName t == opName l) $ bookOps bs