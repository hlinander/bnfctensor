module RenderCalc where

import Data.List
import Data.Ratio

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
mathML StartUp      = ""
mathML StartDown    = ""

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

type FreeIndex = Int
type FreeIndices = [String]
type RenderState = FreeIndices

freeLabels :: [String]
freeLabels = map (\x -> "m" ++ show x) ([0..] :: [Integer])

emptyRenderEnv :: RenderState
emptyRenderEnv = freeLabels

renderConsole :: Calc -> String
renderConsole = flip renderCalc console

renderCalc :: Calc -> (Component -> String) -> String
renderCalc x f = fst $ S.runState (R.runReaderT (renderCalc' f x) emptyRenderEnv) ["a", "b", "c"]

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
    Contract i1 i2 t | i1 > i1 -> undefined
    Number n ->
        return $ (target StartFrac) ++ (show p) ++ (if q == 1 then "" else (target MidFrac) ++ (show q)) ++ (target EndFrac)
      where p = numerator n
            q = denominator n
    Permute p c -> renderCalc' target c
    Tensor name indices -> do
        localFrees <- R.ask
        let open = (target StartTensor)
        let nameString = (target StartIdent) ++ name ++ (target EndIdent)
        let indicesString = concat (take (length indices) localFrees)
        let close = (target EndTensor)
        return $ open ++ nameString ++ indicesString ++ close
    _ -> undefined

reduceTensorDummies :: (Int, b1) -> ([b2], [(b2, b1)]) -> ([b2], [(b2, b1)])
reduceTensorDummies (i, label) (remainingGlobal, pairs) = (newRemaining, newPairs)
    where (newRemaining, localIndex) = popAt i remainingGlobal
          newPairs = pairs ++ [(localIndex, label)]


replaceWithBullets :: Int -> a -> [a] -> [a]
replaceWithBullets idx x xs = lh ++ [x] ++ rh
    where (lh, (r:rh)) = (splitAt idx xs)

renderIndex :: (Component -> String) -> Index -> String -> String
renderIndex target (Index{indexValence=Up}) label = (target IndexPH) ++ mlname
  where mlname = (target StartIdent) ++ (target StartUp) ++ label ++ (target EndIdent)
renderIndex target (Index{indexValence=Down}) label = mlname ++ (target IndexPH)
  where mlname = (target StartIdent) ++ (target StartDown) ++ label ++ (target EndIdent)

-- Should be one line
splitSpaces :: [String] -> [Calc] -> [[String]]
splitSpaces = splitSpaces' []

splitSpaces' :: [[a]] -> [a] -> [Calc] -> [[a]]
splitSpaces' currentOut _ [] = currentOut
splitSpaces' currentOut currentSpace (next:rest) = splitSpaces' nextOut nextSpace remainingFactors
    where
        nextOut = currentOut ++ [take (numFreeSlots next) currentSpace]
        nextSpace = drop (numFreeSlots next) currentSpace
        remainingFactors = rest

numFreeSlots :: Calc -> Int
numFreeSlots x = case x of
    Prod factors -> sum (map numFreeSlots factors)
    Sum terms -> numFreeSlots $ head terms
    Contract _ _ expr -> numFreeSlots expr - 2
    Tensor _ idxs -> length idxs
    Permute _ t -> numFreeSlots t
    _ -> 0