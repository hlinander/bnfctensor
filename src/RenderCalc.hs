module RenderCalc where

import Data.List
import Data.Ratio
import qualified Math.Combinat.Permutations as P

import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S

import Core
import Util
import Debug.Trace

data Component
    = StartOp
    | EndOp
    | Plus
    | Times
    | OpenParen
    | CloseParen
    | StartFrac
    | MidFrac
    | EndFrac
    | StartTensor
    | StartIdent
    | EndIdent
    | EndTensor
    | IndexPH
    | StartUp
    | StartDown
    | EndUp
    | EndDown
    | StartNumber
    | EndNumber

mathML :: Component -> String
mathML StartOp      = "<mrow>\n"
mathML EndOp        = "</mrow>\n"
mathML Plus         = "<mo>+</mo>\n"
mathML Times        = "<mo>⊗</mo>"
mathML OpenParen    = "<mo>(</mo>\n"
mathML CloseParen   = "<mo>)</mo>\n"
mathML StartFrac    = "<mfrac><mi>"
mathML MidFrac      = "</mi><mi>"
mathML EndFrac      = "</mi></mfrac>"
mathML StartTensor  = "<mmultiscripts>\n"
mathML StartIdent   = "<mi>"
mathML EndIdent     = "</mi>"
mathML EndTensor    = "\n</mmultiscripts>"
mathML IndexPH      = "<none/>"
mathML StartUp      = "<mi>"
mathML StartDown    = "<mi>"
mathML EndUp        = "</mi>"
mathML EndDown      = "</mi>"
mathML StartNumber  = "<mn>"
mathML EndNumber    = "</mn>"

console :: Component -> String
console StartOp      = "("
console EndOp        = ")"
console Plus         = " + "
console Times        = " ⊗ "
console OpenParen    = "("
console CloseParen   = ")"
console StartFrac    = "("
console MidFrac      = "/"
console EndFrac      = ")"
console StartTensor  = ""
console StartIdent   = ""
console EndIdent     = ""
console EndTensor    = ""
console IndexPH      = ""
console StartUp      = "^"
console StartDown    = "."
console EndUp      = ""
console EndDown    = ""
console StartNumber  = ""
console EndNumber    = ""

type FreeIndices = [String]
type RenderState = FreeIndices

freeLabels :: [String]
freeLabels = map (:[]) ['a'..'z']

dummyLabels :: [String]
dummyLabels = map (:[]) ['A'..'Z']
-- freeLabels = map (\x -> "d" ++ show x) ([0..] :: [Integer])

emptyRenderEnv :: RenderState
emptyRenderEnv = freeLabels

renderConsole :: Calc -> String
renderConsole = flip renderCalc console

renderCalc :: Calc -> (Component -> String) -> String
renderCalc x f = fst $ S.runState (R.runReaderT (renderCalc' f x) emptyRenderEnv) dummyLabels

getFreesForFactors :: [Int] -> [String] -> [[String]]
getFreesForFactors freeSlots frees = freesFactors
    where reduce (ff, remaining) nFree = (ff ++ [take nFree remaining], drop nFree remaining)
          (freesFactors, _) = foldl reduce ([], frees) freeSlots

renderCalc' :: (Component -> String) -> Calc -> R.ReaderT RenderState (S.State [String]) String
renderCalc' target x = case x of
    Sum terms -> do
        let open = (target StartOp)  ++ (target OpenParen)
        mlterms' <- mapM (renderCalc' target) terms
        let mlterms = intercalate (target Plus) mlterms'
        let close = (target CloseParen) ++ (target EndOp)
        return $ open ++ mlterms ++ close
    Prod (Number n:rest) -> (++) <$> renderCalc' target (Number n) <*> renderCalc' target (Prod rest)
    Prod factors -> do
        let open = (target StartOp)
        frees <- R.ask
        let freeSlots = map numFreeSlots factors
        let freesFactors = getFreesForFactors freeSlots frees
        let factorsWithFrees = zip factors freesFactors
        factors' <- mapM (\(f, localFrees) -> R.local (const localFrees)
            $ renderCalc' target f) factorsWithFrees
        let mlfactors = intercalate (target Times) factors'
        let close = (target EndOp)
        return $ open ++ mlfactors ++ close
    Contract i1 i2 t | i1 < i2 -> do
        oldState <- S.get
        let newState = drop 1 oldState
        let dummy = head oldState
        S.put newState
        frees <- R.ask
        let newFrees = insertAt i2 dummy $ insertAt i1 dummy frees
        R.local (const newFrees) (renderCalc' target t)
    Contract i1 i2 _ | i1 > i2 -> undefined
    Number n ->
        case q of
            1 -> return $ target StartNumber ++ show p ++ target EndNumber
            _ -> return $ target StartFrac ++ show p ++ target MidFrac ++ show q ++ target EndFrac
      where p = numerator n
            q = denominator n
    Permute p c -> do
        localFrees <- R.ask
        let newFrees = P.permuteList p localFrees
        R.local (const newFrees) $ renderCalc' target c
    Tensor name [] -> return $ (target StartIdent) ++ name ++ (target EndIdent)
    Tensor name indices -> do
        localFrees <- R.ask
        let theIndices = zip localFrees indices
        let open = (target StartTensor)
        let nameString = (target StartIdent) ++ name ++ (target EndIdent)
        let indicesString = concatMap (renderIndex target) theIndices --concat (take (length indices) localFrees)
        let close = (target EndTensor)
        return $ open ++ nameString ++ indicesString ++ close
    _ -> undefined

renderIndex :: (Component -> String) -> (String, Index) -> String
renderIndex target (label, Index{indexValence=Up}) = (target IndexPH) ++ mlname
  where mlname = (target StartIdent) ++ (target StartUp) ++ label ++ (target EndUp) ++ (target EndIdent)
renderIndex target (label, Index{indexValence=Down}) = mlname ++ (target IndexPH)
  where mlname = (target StartIdent) ++ (target StartDown) ++ label ++ (target EndDown) ++ (target EndIdent)

numFreeSlots :: Calc -> Int
numFreeSlots x = case x of
    Prod factors -> sum (map numFreeSlots factors)
    Sum terms -> numFreeSlots $ head terms
    Contract _ _ expr -> numFreeSlots expr - 2
    Tensor _ idxs -> length idxs
    Permute _ t -> numFreeSlots t
    _ -> 0