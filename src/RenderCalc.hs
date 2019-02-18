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
console StartOp      = ""
console EndOp        = ""
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
freeLabels = map (:['\x0332']) ['a'..'z']

dummyLabels :: [String]
dummyLabels = map (:[]) ['a'..'z']

emptyRenderEnv :: RenderState
emptyRenderEnv = freeLabels

renderConsole :: Calc -> String
renderConsole = flip renderCalc console

renderCalc :: Calc -> (Component -> String) -> String
renderCalc x f = fst $ S.runState (R.runReaderT (renderCalc' 0 f x) emptyRenderEnv) dummyLabels

getFreesForFactors :: [Int] -> [String] -> [[String]]
getFreesForFactors freeSlots frees = freesFactors
    where reduce (ff, remaining) nFree = (ff ++ [take nFree remaining], drop nFree remaining)
          (freesFactors, _) = foldl reduce ([], frees) freeSlots

sumPrec = 4
prodPrec = 5
opPrec = 6

renderParen pred target x = if pred then (target OpenParen) ++ x ++ (target CloseParen) else x
renderOp target x = (target StartOp) ++ x ++ (target EndOp)
renderIdent target x = (target StartIdent) ++ x ++ (target EndIdent)
renderUp target x = (target StartUp) ++ x ++ (target EndUp)
renderDown target x = (target StartDown) ++ x ++ (target EndDown)

renderCalc' :: Int -> (Component -> String) -> Calc -> R.ReaderT RenderState (S.State [String]) String
renderCalc' prec target x = case x of
    Sum s1 s2 -> do
        rs1 <- (renderCalc' sumPrec target) s1
        rs2 <- (renderCalc' sumPrec target) s2
        let wrap = renderOp target . renderParen (prec > sumPrec) target
        return $ wrap (rs1 ++ (target Plus) ++ rs2)
    -- Prod (Number n) f2 -> (++) <$> renderCalc' prec target (Number n) <*> renderCalc' prec target f2
    Prod f1 f2 -> do
        frees <- R.ask
        let freeSlots = map numFreeSlots [f1, f2]
        let [free1, free2] = getFreesForFactors freeSlots frees
        rf1 <- R.local (const free1) $ renderCalc' prodPrec target f1
        rf2 <- R.local (const free2) $ renderCalc' prodPrec target f2
        let wrap = renderOp target . renderParen (prec > prodPrec) target
        return $ wrap (rf1 ++ (target Times) ++ rf2)
    Contract i1 i2 t | i1 < i2 -> do
        oldState <- S.get
        let newState = drop 1 oldState
        let dummy = head oldState
        S.put newState
        frees <- R.ask
        let newFrees = insertAt i2 dummy $ insertAt i1 dummy frees
        R.local (const newFrees) (renderCalc' prec target t)
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
        R.local (const newFrees) $ renderCalc' prec target c
    Tensor name [] -> return $ renderIdent target name
    Tensor name indices -> do
        localFrees <- R.ask
        let theIndices = zip localFrees indices
        let open = (target StartTensor)
        let indicesString = concatMap (renderIndex target) theIndices
        let close = (target EndTensor)
        return $ open ++ renderIdent target name ++ indicesString ++ close
    Op name [] calc -> do
        calcString <- renderCalc' opPrec target calc
        return $ renderIdent target name ++ calcString
    Op name indices calc -> do
        localFrees <- R.ask
        let opPreIndices = take (length indices) localFrees
        let newLocalFrees = drop (length indices) localFrees
        calcString <- R.local (const newLocalFrees) $ renderCalc' opPrec target calc
        let opIndices_ = zip opPreIndices indices
        let open = (target StartTensor)
        let nameString = (target StartIdent) ++ name ++ (target EndIdent)
        let indicesString = concatMap (renderIndex target) opIndices_
        let close = (target EndTensor)
        return $ open ++ nameString ++ indicesString ++ close ++ calcString
    _ -> undefined

renderIndex :: (Component -> String) -> (String, Index) -> String
renderIndex target (label, Index{indexValence=Up}) = (target IndexPH) ++ (renderIdent target $ renderUp target label)
renderIndex target (label, Index{indexValence=Down}) = (renderIdent target $ renderDown target label) ++ (target IndexPH)

numFreeSlots :: Calc -> Int
numFreeSlots x = case x of
    Prod f1 f2 -> numFreeSlots f1 + numFreeSlots f2
    Sum s1 s2 -> numFreeSlots s1
    Contract _ _ expr -> numFreeSlots expr - 2
    Tensor _ idxs -> length idxs
    Permute _ t -> numFreeSlots t
    Op _ idxs c -> (length idxs) + numFreeSlots c
    _ -> 0

