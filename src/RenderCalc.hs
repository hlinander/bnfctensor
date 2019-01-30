module RenderCalc where

import Data.List
import Data.Ratio

import qualified Control.Monad.Reader as R

import Core
import Util

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
type ContractType = (FreeIndex, String)
type Offset = Int
type RenderState = ([ContractType], Offset)

freeLabels :: [String]
freeLabels = map (\x -> "m" ++ show x) ([0..] :: [Integer])

renderCalc :: (Component -> String) -> (String -> String) -> Calc -> R.Reader RenderState String
renderCalc target indent x = case x of
    Sum terms -> do
        let open = indent (target StartOp)  ++ (indent.indent) (target OpenParen)
        mlterms' <- mapM (renderCalc target (indent.indent)) terms
        let mlterms = intercalate ((indent.indent) (target Plus)) mlterms'
        let close = (indent.indent) (target CloseParen) ++ indent (target EndOp)
        return $ open ++ mlterms ++ close
    Prod factors -> do
        let open = indent (target StartOp)
        (contractions, offset) <- R.ask
        let freeSlots = map numFreeSlots factors
        let offsets = init $ scanl (+) 0 freeSlots
        let factorsOffsets = zip factors offsets
        factors' <- mapM (\(f, ns) -> R.local (const (contractions, offset + ns))
            $ renderCalc target (indent.indent) f) factorsOffsets
        let mlfactors = intercalate (target Times) factors'
        let close = indent (target EndOp)
        return $ open ++ mlfactors ++ close
    Contract i1 i2 t -> do
        (contractions, offset) <- R.ask
        let newContractions = contractions ++ [(i1, "x"),(i2, "x")]
        R.local (const (newContractions, offset)) (renderCalc target indent t)
    Number n ->
        return $ indent (target StartFrac) ++ (show p) ++ (if q == 1 then "" else (target MidFrac) ++ (show q)) ++ (target EndFrac)
      where p = numerator n
            q = denominator n
    Tensor name indices -> do
        (contractions, offset) <- R.ask
        let open = indent (target StartTensor)
        let mlname = indent (target StartIdent) ++ name ++ (target EndIdent)
        -- List with other free (and possibly contracted in other factors)
        -- indices, this tensors indices (in local index
        -- positions) and an infinite continuation.
        -- Enables the conversion from contraction indices (relative all free
        -- indices) to local indices (w.r.t this tensor)
        -- I.e. a tensor with 3 indices, preceded by 2 other free indices in expression would look like
        -- [(-1),(-1),0,1,2,(-1),(-1),...]
        -- for example T.a.d * S^a^b^c when at the node for S.
        let globalIndices = (replicate offset (-1)) ++ [0..(length indices - 1)] ++ repeat (-1)
        -- Create a list of contracted indices together with their local index
        let (_, dummies) = foldr reduceTensorDummies (globalIndices, []) contractions
        -- Filter for indices pertaining to this tensor
        let relevantDummies = filter ((0 <=).fst) dummies
        let dummyPositions = map fst dummies
        -- Free indices are the complement of the dummies
        let frees = [0..(length indices - 1)] \\ dummyPositions
        let freeOffset = length $ filter (\(i, _) -> i < offset) contractions
        -- Associate labels with the free indices
        let freesWithLabels = zip frees (drop (offset - freeOffset) freeLabels)
        let allIndices_ = freesWithLabels ++ relevantDummies
        -- Sort in ascending in index position
        let allIndices = sortBy (\(a, _) (b, _) -> compare a b) allIndices_
        -- Get labels
        let mlindices = map snd allIndices
        let close = indent (target EndTensor)
        return $ open ++ mlname ++ concat mlindices ++ close
    _ -> undefined

reduceTensorDummies :: (Int, b1) -> ([b2], [(b2, b1)]) -> ([b2], [(b2, b1)])
reduceTensorDummies (i, label) (remainingGlobal, pairs) = (newRemaining, newPairs)
    where (newRemaining, localIndex) = popAt i remainingGlobal
          newPairs = pairs ++ [(localIndex, label)]


replaceWithBullets :: Int -> a -> [a] -> [a]
replaceWithBullets idx x xs = lh ++ [x] ++ rh
    where (lh, (r:rh)) = (splitAt idx xs)

renderIndex :: (Component -> String) -> (String -> String) -> Index -> String -> String
renderIndex target indent (Index{indexValence=Up}) label = (target IndexPH) ++ mlname
  where mlname = (target StartIdent) ++ (target StartUp) ++ label ++ (target EndIdent)
renderIndex target indent (Index{indexValence=Down}) label = mlname ++ (target IndexPH)
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
    _ -> 0