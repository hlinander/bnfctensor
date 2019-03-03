module Debug where

import Frontend.AbsTensor
import Frontend.ErrM
import Main
import Core
import Control.Monad.Reader
import CalcFromExpr
-- import GHC.Real (:%)


getExpr :: Err Book -> Expr
getExpr (Ok (Derivation [StmtVoid expr])) = expr
getExpr _ = undefined

debugParse :: String -> Expr
debugParse = getExpr . parse

debugCalc :: String -> Calc
debugCalc string = case calcFromExpr (debugParse string) emptyBook of
    Left foo -> undefined
    Right bar -> bar

-- debugCalcReal :: Calc -> String

--   mathml (EOp Plus terms) indent = open ++ mlterms ++ close
--     where open = indent "<mrow>\n" ++ (indent.indent) "<mo>(</mo>\n"
--           mlterms = intercalate ((indent.indent) "<mo>+</mo>\n") (map (\x -> mathml x (indent.indent)) terms)
--           close = (indent.indent) "<mo>)</mo>\n" ++ indent "</mrow>\n"