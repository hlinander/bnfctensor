module Debug where

import Frontend.AbsTensor
import Frontend.ErrM
import Main
import Core
import Control.Monad.Reader
import CalcFromExpr
import System.IO.Unsafe
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

debugCalcBS :: String -> BookState -> Calc
debugCalcBS string bs = case calcFromExpr (debugParse string) bs of
    Left foo -> undefined
    Right bar -> bar

debugBSAndCalc :: String -> String -> (BookState, Calc)
debugBSAndCalc sbs sc = unsafePerformIO (repl' emptyBook sbs >>= \(bs, _) -> return $ (bs, debugCalcBS sc bs))

loadBSFromFile :: FilePath -> IO BookState
loadBSFromFile name = do
    content <- readFile name
    return $ read content

-- debug calc = unsafePerformIO $ putStrLn (renderConsole calc) >> return calc

-- debugCalcReal :: Calc -> String

--   mathml (EOp Plus terms) indent = open ++ mlterms ++ close
--     where open = indent "<mrow>\n" ++ (indent.indent) "<mo>(</mo>\n"
--           mlterms = intercalate ((indent.indent) "<mo>+</mo>\n") (map (\x -> mathml x (indent.indent)) terms)
--           close = (indent.indent) "<mo>)</mo>\n" ++ indent "</mrow>\n"