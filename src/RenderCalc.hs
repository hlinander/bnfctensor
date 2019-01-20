module RenderCalc where

import Data.List
import Data.Ratio

import Core

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
mathML Times        = ""
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
mathML StartDown      = ""

console :: Component -> String
console StartOp      = ""
console EndOp        = ""
console Plus         = " + "
console Times        = " "
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
console StartDown      = "."

renderCalc :: (Component -> String) -> (String -> String) -> Calc -> String
renderCalc target indent x = case x of
  Op (:+) terms -> open ++ mlterms ++ close
    where open = indent (target StartOp)  ++ (indent.indent) (target OpenParen)
          mlterms = intercalate ((indent.indent) (target Plus)) (map renderTerm terms)
          close = (indent.indent) (target CloseParen) ++ indent (target EndOp)
          renderTerm x = renderCalc target (indent.indent) x
  Op (:*) factors -> open ++ mlfactors ++ close
    where open = indent (target StartOp)
          mlfactors = intercalate (target Times) (map (renderCalc target (indent.indent)) factors)
          close = indent (target EndOp)
  Op (Contract i1 i2) (t:[]) -> renderCalc target indent t
  Number n -> indent (target StartFrac) ++ (show p) ++ (target MidFrac) ++ (show q) ++ (target EndFrac)  
    where p = numerator n
          q = denominator n
  Tensor name indices perm -> open ++ mlname ++ mlindices ++ close
    where open = indent (target StartTensor)
          mlname = indent (target StartIdent) ++ name ++ (target EndIdent)
          mlindices = intercalate "" $ map (renderIndex target (indent.indent)) (reverse indices) 
          close = indent (target EndTensor)

renderIndex :: (Component -> String) -> (String -> String) -> Index -> String
renderIndex target indent (Index{indexValence=Up}) = (target IndexPH) ++ mlname
  where mlname = (target StartIdent) ++ (target StartUp) ++ "0" ++ (target EndIdent) 
renderIndex target indent (Index{indexValence=Down}) = mlname ++ (target IndexPH)
  where mlname = (target StartIdent) ++ (target StartDown) ++ "0" ++ (target EndIdent)