{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


module RenderCalc (
    renderConsole,
    renderMathML,
    Printable(..),
    PrintMode(..),
    printCalc
) where

import Data.Ratio
import qualified Math.Combinat.Permutations as P

import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S

import Core
import Util
import Prelude hiding ( print )
import Frontend.AbsTensor ( DocString(..) )

data PrintMode = MathML | Console

printCalc :: Printable p => PrintMode -> p -> String
printCalc MathML o = (print MathML o) ++ script
printCalc Console o = (print Console o)

-- (script++) . (css++) .
print :: Printable p => PrintMode -> p -> String
print MathML = printML
print Console = printConsole

class Printable p where
    printML :: p -> String
    printConsole :: p -> String

instance Printable Calc where
    printML = renderMathML
    printConsole = renderConsole

instance Printable String where
    printML s = "<span style=\"white-space: pre; font-family: monospace;\">" ++ s ++ "</span>"
    printConsole = id

instance Printable DocString where
    printML (DocString s) = "<strong>" ++ s ++ "</strong><br>"
    printConsole (DocString s) = s

renderConsole :: Calc -> String
renderConsole = flip renderCalc Console

renderMathML :: Calc -> String
renderMathML c = "<math>" ++ (renderCalc c MathML) ++ "</math><br>"

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
    | TensorIdent String String
    | IndexIdent ValenceType String String

script = "<script>registerTensorHover();</script>"

niceLabelML s = case s of
    "eta" -> "&eta;"
    "delta" -> "&delta;"
    s -> s

niceLabelConsole s = case s of
    "eta" -> "η"
    "delta" -> "ẟ"
    s -> s


mathML :: Component -> String
mathML StartOp      = "<mrow>\n"
mathML EndOp        = "</mrow>\n"
mathML Plus         = "<mo>+</mo>\n"
--mathML Times        = "<mo>⊗</mo>"
mathML Times        = "<mo></mo>"
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
mathML (TensorIdent cs l) = "<mi mathvariant=\"bold\" class=\"tensor " ++ l ++ "\">" ++ niceLabelML l ++ "</mi>"
mathML (IndexIdent Up cs l) = "<none/><mi>" ++ l ++ "</mi>"
mathML (IndexIdent Down cs l) = "<mi>" ++ l ++ "</mi><none/>"

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
console (TensorIdent cs l) = niceLabelConsole l
console (IndexIdent Up cs l) = "^" ++ niceLabelConsole l
console (IndexIdent Down cs l) = "." ++ niceLabelConsole l

instance Printable Component where
    printML = mathML
    printConsole = console

type FreeIndices = [String]
type RenderState = FreeIndices

infLabels = map (("a"++).show) ([1..] :: [Int])

freeLabels :: [String]
freeLabels = (map (:['\x0332']) ['a'..'z']) ++ infLabels

dummyLabels :: [String]
dummyLabels = (map (:[]) $ reverse ['a'..'z']) ++ infLabels

emptyRenderEnv :: RenderState
emptyRenderEnv = freeLabels

renderCalc :: Calc -> PrintMode -> String
renderCalc x m = fst $ S.runState (R.runReaderT (renderCalc' 0 m x) emptyRenderEnv) dummyLabels

getFreesForFactors :: [Int] -> [String] -> [[String]]
getFreesForFactors freeSlots frees = freesFactors
    where reduce (ff, remaining) nFree = (ff ++ [take nFree remaining], drop nFree remaining)
          (freesFactors, _) = foldl reduce ([], frees) freeSlots

sumPrec = 4
prodPrec = 5
opPrec = 6

renderParen pred mode x = if pred then (print mode OpenParen) ++ x ++ (print mode CloseParen) else x
renderOp mode x = (print mode StartOp) ++ x ++ (print mode EndOp)

renderCalc' :: Int -> PrintMode -> Calc -> R.ReaderT RenderState (S.State [String]) String
renderCalc' prec mode x = case x of
    Sum s1 s2 -> do
        rs1 <- (renderCalc' sumPrec mode) s1
        rs2 <- (renderCalc' sumPrec mode) s2
        let wrap =  renderOp mode . renderParen (prec > sumPrec) mode
        return $ wrap (rs1 ++ (print mode Plus) ++ rs2)
    -- Prod (Number n) f2 -> (++) <$> renderCalc' prec target (Number n) <*> renderCalc' prec target f2
    Prod f1 f2 -> do
        frees <- R.ask
        let freeSlots = map numFreeSlots [f1, f2]
        let [free1, free2] = getFreesForFactors freeSlots frees
        rf1 <- R.local (const free1) $ renderCalc' prodPrec mode f1
        rf2 <- R.local (const free2) $ renderCalc' prodPrec mode f2
        let wrap = renderOp mode . renderParen (prec > prodPrec) mode
        return $ wrap (rf1 ++ (print mode Times) ++ rf2)
    Contract i1 i2 t | i1 < i2 -> do
        oldState <- S.get
        let newState = drop 1 oldState
        let dummy = head oldState
        S.put newState
        frees <- R.ask
        let newFrees = insertAt i2 dummy $ insertAt i1 dummy frees
        R.local (const newFrees) (renderCalc' prec mode t)
    Contract i1 i2 c | i1 > i2 -> renderCalc' prec mode (Contract i2 i1 c)
    Number n ->
        case q of
            1 -> return $ target StartNumber ++ show p ++ target EndNumber
            _ -> return $ target StartFrac ++ show p ++ target MidFrac ++ show q ++ target EndFrac
      where p = numerator n
            q = denominator n
            target = print mode
    Permute p c -> do
        localFrees <- R.ask
        let newFrees = P.permuteList p localFrees
        R.local (const newFrees) $ renderCalc' prec mode c
    Tensor name [] -> return $ print mode (TensorIdent name name)
    Tensor name indices -> do
        localFrees <- R.ask
        let theIndices = zip localFrees indices
        let open = (print mode StartTensor)
        let indicesString = concatMap (renderIndex mode) theIndices
        let close = (print mode EndTensor)
        return $ open ++ print mode (TensorIdent name name) ++ indicesString ++ close
    Op name [] calc -> do
        calcString <- renderCalc' opPrec mode calc
        return $ print mode (TensorIdent name name) ++ calcString
    Op name indices calc -> do
        localFrees <- R.ask
        let opPreIndices = take (length indices) localFrees
        let newLocalFrees = drop (length indices) localFrees
        calcString <- R.local (const newLocalFrees) $ renderCalc' opPrec mode calc
        let opIndices_ = zip opPreIndices indices
        let open = (print mode StartTensor)
        let nameString = print mode (TensorIdent name name)
        let indicesString = concatMap (renderIndex mode) opIndices_
        let close = (print mode EndTensor)
        return $ open ++ nameString ++ indicesString ++ close ++ calcString
    _ -> undefined

renderIndex :: PrintMode -> (String, Index) -> String
renderIndex mode (label, Index{indexValence=Up}) = (print mode (IndexIdent Up label label))
renderIndex mode (label, Index{indexValence=Down}) = (print mode (IndexIdent Down label label))
-- renderIndex mode (label, Index{indexValence=Up}) = (print mode IndexPH) ++ (print mode (IndexIdent label label))
-- renderIndex mode (label, Index{indexValence=Down}) = (print mode (IndexIdent label label)) ++ (print mode IndexPH)

numFreeSlots :: Calc -> Int
numFreeSlots x = case x of
    Prod f1 f2 -> numFreeSlots f1 + numFreeSlots f2
    Sum s1 _ -> numFreeSlots s1
    Contract _ _ expr -> numFreeSlots expr - 2
    Tensor _ idxs -> length idxs
    Permute _ t -> numFreeSlots t
    Op _ idxs c -> (length idxs) + numFreeSlots c
    _ -> 0

