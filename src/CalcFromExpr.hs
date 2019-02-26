module CalcFromExpr where

import Math.Combinat.Permutations
import Control.Monad.Reader
import Control.Monad.Except
import Data.Bifunctor
import Data.Ratio
import Data.List
import Data.Foldable
import Debug.Trace

import qualified Frontend.AbsTensor as Abs
import Core
import qualified Tensor as T
import Util
import Transform

calcFromExpr :: Abs.Expr -> BookState -> Either String Calc
calcFromExpr x bs = second fst $ runExcept $ runReaderT (calcFromExpr' x) bs

calcFromExpr' :: Abs.Expr -> (ReaderT BookState (Except String)) (Calc, [(String, Index)])
calcFromExpr' x = case x of
  Abs.Add expr1 expr2 -> do
    (calc1, idx1) <- calcFromExpr' expr1
    (calc2, _) <- calcFromExpr' expr2
    return (calc1 |+| calc2, idx1)
  Abs.Sub expr1 expr2 -> do
    (calc1, idx1) <- calcFromExpr' expr1
    (calc2, _) <- calcFromExpr' expr2
    return (calc1 |+| Number (-1) |*| calc2, idx1)
  Abs.Neg expr -> do
    (calc, idx) <- calcFromExpr' expr
    return (Number (-1) |*| calc, idx)
  Abs.Div expr (Abs.Number num) -> do
    (calc, idx) <- calcFromExpr' expr
    return (calc |*| Number (1 % num), idx)
  Abs.Div expr (Abs.Fraction p q) -> do
    (calc, idx) <- calcFromExpr' expr
    return (calc |*| Number (p % q), idx)
  Abs.Div _ _ -> undefined
  Abs.Tensor (Abs.Label l) absIdx -> do
    maybeCalc <- asks (lookupCalc l)
    tensorType <- asks (lookupTensor' ("Undefined tensor '" ++ l ++ "'") l) >>= liftEither
    bs <- ask
    let indexTypes = tensorIndices tensorType
    let freeSlots = T.freeIndexPos x
    let (indices, perm, contractions, sortedIdxAndLabels) = processIndexedObject absIdx indexTypes freeSlots
    calc <- case maybeCalc of
      Just storedCalc -> liftEither $ newCalc
          where storedIndices = indexFromCalc storedCalc
                valences = map indexValence storedIndices
                requestedValences = map absIndexValence absIdx
                needsFix = filter (\(v, rv, _) -> v /= rv) (zip3 valences requestedValences [0..])
                positions = map (\(_, _, i) -> i) needsFix
                newCalc = foldlM (changeValence bs) storedCalc positions
      Nothing -> return $ Tensor l indices
    let contractedCalc = contractNew contractions calc
    let res = if isIdentityPermutation perm
                then contractedCalc
                else Permute perm contractedCalc
    return (res, sortedIdxAndLabels)
  Abs.Anon i absIdx -> do
    let varName = '$' : show i
    maybeCalc <- asks (lookupCalc' ("Undefined expression identifier '$" ++ show i ++ "'") varName) >>= liftEither
    tensorType <- asks (lookupTensor' ("Undefined tensor identifier '$" ++ show i ++ "'") varName) >>= liftEither
    let indexTypes = tensorIndices tensorType
    let freeSlots = T.freeIndexPos x
    let (indices, perm, contractions, sortedIdxAndLabels) = processIndexedObject absIdx indexTypes freeSlots
    --let calc = maybeCalc
    let contractedCalc = contractNew contractions maybeCalc
    let res = if isIdentityPermutation perm
                then contractedCalc
                else Permute perm contractedCalc
    return (res, sortedIdxAndLabels)
    --case maybeCalc of
    --  Nothing -> lift Nothing
    --  Just storedCalc -> do
  Abs.Op (Abs.Label l) absIdx expr -> do
    (calc, idx) <- calcFromExpr' expr
    opType <- asks (lookupOp' ("Undefined operator '" ++ l ++ "'") l) >>= liftEither
    let calcAbsIndices = map labeledIndexToAbsIndex idx
    let indexTypes = opIndices opType ++ indexTypeFromCalc calc
    let allIndices = absIdx ++ calcAbsIndices
    let freeSlots = T.freeIndicesWithPos allIndices []
    let (indices, perm, contractions, sortedIdxAndLabels) = processIndexedObject allIndices indexTypes freeSlots
    let contractedCalc = contractNew contractions (Op l (take (length absIdx) indices) calc)
    let permCalc = if isIdentityPermutation perm
                    then contractedCalc
                    else Permute perm contractedCalc
    return (permCalc, sortedIdxAndLabels)
  Abs.Number p -> return (Number (fromInteger p), [])
  Abs.Fraction p q -> return (Number (p % q), [])
  Abs.Mul expr1 expr2 -> do
    (calc1, idx1) <- calcFromExpr' expr1
    (calc2, idx2) <- calcFromExpr' expr2
    let offset = length (T.freeIndexSlots expr1)
    let (perm, contracted, f') = processBinaryIndexed idx1 idx2 offset
    let res = if isIdentityPermutation perm
                then (contractNew contracted $ calc1 |*| calc2, f')
                else (Permute perm $ contractNew contracted $ calc1 |*| calc2, f')
    return $ res
  Abs.Func (Abs.Label name) (expr:[]) -> do
    (calc, idx) <- calcFromExpr' expr
    return $ (execute name calc, idx)

  x -> (return $ (traceShow ("vinsten: " ++ show x) (Number 1), []))

labeledIndexToAbsIndex (l, Index{indexValence=Up}) = Abs.Upper (Abs.Label l)
labeledIndexToAbsIndex (l, Index{indexValence=Down}) = Abs.Lower (Abs.Label l)

type LabeledIndex = (String, Index)
type AbsIndexPos = (Abs.Index, Int)
type IndexedData = ([Index], Permutation, [ContractPairNew], [LabeledIndex])

processIndexedObject :: [Abs.Index] -> [IndexType] -> [AbsIndexPos] -> IndexedData
processIndexedObject absIdx indexTypes freeSlots = (indices, perm, contractions, sortedIdxAndLabels)
    where absIndicesWithType = zip absIdx $ indexTypes :: [(Abs.Index, IndexType)]
          indices = map (uncurry indexTypeToIndex) absIndicesWithType :: [Index]
          -- Which of the indices are free?
          freeSlotsPos = map snd freeSlots :: [Int]
          freeSlotsLabels = map (indexLabel.fst) freeSlots :: [String]
          freeIndices = map (indices!!) freeSlotsPos :: [Index]
          freeIndicesAndLabels = zip freeSlotsLabels freeIndices :: [(String, Index)]
          sortedIdxAndLabels = sortBy (\(a, _) (b, _) -> compare a b) freeIndicesAndLabels
          perm = inverse $ sortingPermutationAsc freeSlotsLabels
          contractions = selfContractedIndexPairs (zip absIdx indices)

type BinaryIndexedData = (Permutation, [ContractPairNew], [LabeledIndex])

processBinaryIndexed :: [LabeledIndex] -> [LabeledIndex] -> Int -> BinaryIndexedData
processBinaryIndexed idx1 idx2 offset = (perm, contracted, f')
    where contracted = contractedPairsNew idx1 idx2
          c1 = map (\(_,_,i) -> i) $ map fst contracted
          c2 = map (\(_,_,i) -> i) $ map snd contracted
          c2' = map (\i -> i - offset) c2
          f1 = foldr deleteAt idx1 c1
          f2 = foldr deleteAt idx2 c2'
          f = map fst $ f1 ++ f2
          perm = inverse $ sortingPermutationAsc f
          f' = permuteList (inverse $ perm) (f1 ++ f2)

-- Create nested contractions for a list of contraction pairs of slots
contractNew :: [ContractPairNew] -> Calc -> Calc
contractNew [] expr = expr
contractNew (((_, _, i1), (_, _, i2)):rest) expr = Contract i1 i2 $ contractNew rest expr

type IndexSlotNew = (String, Index, Int)
type ContractPairNew = (IndexSlotNew, IndexSlotNew)

labelEqual :: ContractPairNew -> Bool
labelEqual ((l1, _, _),(l2, _, _)) = l1 == l2

-- labelEqual :: ContractPair -> Bool
-- labelEqual ((l1, _),(l2, _)) = indexLabel l1 == indexLabel l2

contractedPairsNew:: [(String, Index)] -> [(String, Index)] -> [ContractPairNew]
contractedPairsNew free1 free2 = nestedPairs
    where -- Get indices with same label between factors
          free1Pos = map (\((l, index), idx) -> (l, index, idx)) $ zip free1 [0..]
          free2Pos = map (\((l, index), idx) -> (l, index, idx)) $ zip free2 [0..]
          cartProd = [(i1, i2) | i1 <- free1Pos, i2 <- free2Pos]
          intersection = filter (\((l1, _, _), (l2, _, _)) -> l1 == l2) cartProd
          (lh, rh) = (map fst intersection, map snd intersection)
          -- Offset the right hand factor
          orh = [(l, index, i + length free1) | (l, index, i) <- rh]
          -- Generate contraction pairs
          pairs = zip lh orh
          -- Nest contractions
          nestedPairs = getNestedPairsNew pairs (length free1 + length free2)

getNestedPairsNew :: [ContractPairNew] -> Int -> [ContractPairNew]
getNestedPairsNew pairs n = newPairs
    where slots = [0..n]
          (_, newPairs) = foldr reduceNestedPairNew (slots, []) pairs

reduceNestedPairNew :: ContractPairNew -> ([Int], [ContractPairNew]) -> ([Int], [ContractPairNew])
reduceNestedPairNew ((l1, index1, i1), (l2, index2, i2)) (oldPos, newContractions)
    | i1 < i2 = (newPos, ((l1, index1, pos1), (l2, index2, pos2)):newContractions)
     where pos1 = unsafeMaybe $ elemIndex i1 oldPos
           pos2 = unsafeMaybe $ elemIndex i2 oldPos
           (spos1, spos2) = case pos2 > pos1 of
             True -> (pos1, pos2)
             False -> (pos2, pos1)
           (oldPos', _) = popAt spos2 oldPos
           (newPos, _) = popAt spos1 oldPos'

selfContractedIndexPairs :: [(Abs.Index, Index)] -> [ContractPairNew]
selfContractedIndexPairs idxs = nestedPairs
    where indexSlotPositions = zip idxs [0..length idxs]
          indexFormat = map (\((absindex, index), i) -> (indexLabel absindex, index, i)) indexSlotPositions
          distributedPairs = [
                (i1, i2) |
                i1@(_, _, x) <- indexFormat,
                i2@(_, _, y) <- indexFormat,
                x < y
           ]
          intersection = filter labelEqual distributedPairs
          nestedPairs = getNestedPairsNew intersection (length idxs)

indexTypeToIndex:: Abs.Index -> IndexType -> Index
indexTypeToIndex av r@ReprType{reprDim=d,reprGroup=g} = Index r v
            where v = calcVal av
                  calcVal (Abs.Upper _) = Up
                  calcVal (Abs.Lower _) = Down

indexLabel :: Abs.Index -> String
indexLabel (Abs.Upper (Abs.Label lbl)) = lbl
indexLabel (Abs.Lower (Abs.Label lbl)) = lbl

absIndexValence :: Abs.Index -> ValenceType
absIndexValence (Abs.Upper _) = Up
absIndexValence (Abs.Lower _) = Down