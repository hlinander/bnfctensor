{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module XPerm where

import System.IO()
import System.Environment
import Foreign.Marshal.Array
import System.IO.Unsafe

import Foreign.C.Types
import qualified Language.C.Inline.Cpp as C

import Math.Combinat.Permutations
import Data.List

import Core
import Util
import RenderCalc

C.context C.cppCtx

C.include "external/xperm_new.cc"

toCInt = map fromIntegral

xPerm :: [Int] -- ^ Permutation to canonicalise
      -> [[Int]] -- ^ Generating set
      -> [Int] -- ^ Base
      -> [Int] -- ^ Free indices
      -> [Int] -- ^ Dummy indices
      -> [Int]
xPerm perm gs base frees dummies = unsafePerformIO $ do
  result <- callXPerm (toCInt perm) (map toCInt gs) (toCInt base) (toCInt frees) (toCInt dummies)
  return $ map fromIntegral result

callXPerm :: [CInt] -- ^ Permutation to canonicalise
          -> [[CInt]] -- ^ Generating set
          -> [CInt] -- ^ Base
          -> [CInt] -- ^ Free indices
          -> [CInt] -- ^ Dummy indices
          -> IO [CInt]
callXPerm perm gs base frees dummies = allocaArray (length perm) $ \ptr ->
  withArrayLen (concat gs) $ \gsLen gsPtr ->
  withArrayLen base $ \baseLen basePtr -> 
  withArrayLen perm $ \permLen permPtr ->
  withArrayLen dummies $ \dummyLen dummyPtr ->
  withArrayLen frees $ \freeLen freePtr -> do
    let cgsLen = fromIntegral gsLen
        cbaseLen = fromIntegral baseLen
        cpermLen = fromIntegral permLen
        cfreeLen = fromIntegral freeLen
        cdummyLen = fromIntegral dummyLen
        ngs = fromIntegral (length gs) in
      [C.exp| void{
        int vds[1] = {$(int cdummyLen)};
        int mQ[1] = {1};
        canonical_perm_ext(
          $(int *permPtr),
          $(int cpermLen),
          0, // No SGS, only generating set
          $(int *basePtr),
          $(int cbaseLen),
          $(int *gsPtr),
          $(int ngs),
          $(int *freePtr),
          $(int cfreeLen),
          vds, // vds: List of lengths of dummies
          1, // vdsl: Length of previous
          $(int *dummyPtr), // dummies: List of pairs of dummies
          $(int cdummyLen), // dl: 
          mQ, // mQ:
          0, // vrs:
          0, // vrsl: 
          0, // repes: Repeated indices
          0, // rl:
          $(int *ptr)
        );
      }|]
    peekArray (length perm) ptr

testxperm = xPerm [2, 1, 4, 3, 5, 6] [[2, 1, 3, 4, 6, 5], [1, 2, 4, 3, 6, 5]] [1] [1, 2, 3, 4] []

type PermutationList = [Int]
type DummyNames = [Int]
-- [0 1 2 3 4 5]
--      ^   ^
type Positions = [Int]
type AbsoluteContractions = [Int]
type ContractState = (Positions, AbsoluteContractions)
emptyContractState n = ([0..n], []) :: ContractState

colCont :: Calc -> [Int] -> ([Int], [Int], Calc)
colCont c p = colCont' c p []

colCont' :: Calc -> [Int] -> [Int] -> ([Int], [Int], Calc)
colCont' (Contract i1 i2 c) lperm dummies = colCont' c p' (dummies ++ [n1, n2])
          where n2 = nFreeIndices c
                n1 = n2 - 1
                p' = insertAt i2 n2 $ insertAt i1 n1 lperm
colCont' c lperm dummies = (lperm, dummies, c)

reduceContractions c perm [] = Permute (toPermutation perm) c
reduceContractions c perm (d1:d2:rest) = reduceContractions (Contract i1 i2 c) perm' rest
  where i1 = unsafeElemIndex d1 perm
        i2 = unsafeElemIndex d2 perm
        [si1, si2] = sort [i1, i2]
        perm' = deleteAt si1 $ deleteAt si2 perm

xPermToCalc :: Calc -> [Int] -> [Int] -> Calc
xPermToCalc c xperm dummies = Prod sign $ reduceContractions c lperm dummies
  where lperm = take (length xperm - 2) xperm
        [s1, s2] = drop (length xperm - 2) xperm
        sign = if s2 > s1 then Number 1 else Number (-1)

xPermData :: Calc -> (PermutationList, DummyNames, Calc)
xPermData (Permute p c) = colCont c (fromPermutation p)
xPermData c = colCont c [1..(nFreeIndices c)]

-- When we have a stack of Permute Contract Contract ... Contract Prod Prod ... Prod
-- the contract indices already refers to the "names" (see xperm_new.cc L2315) of the dummies
debug calc = unsafePerformIO $ putStrLn (renderConsole calc) >> return calc

-- canonXPerm :: Calc -> Calc
-- canonXPerm (Sum s1 s2) = Sum (canonXPerm s1) (canonXPerm s2)
-- canonXPerm c = xPermToCalc cfree (traceShowId cxperm) (dummies)-- traceShow (show (lperm, frees, dummies, gs, cperm, cxperm)) (debug c)
--   where frees = [1..(nFreeIndices c)]
--         (lperm, dummies, cfree) = xPermData c
--         lperm' = lperm ++ [length lperm + 1, length lperm + 2]
--         gs = generatingSet cfree
--         cxperm = xPerm lperm' gs [1..(nFreeIndices cfree)] frees dummies

-- [1 2 5 3 4 6] [5 6]

collectContractions :: Calc -> ([Int], Calc)
collectContractions c = collectContractions' c []

-- [c b d a d e]
--  3 2   1   4
-- The above perm is exactly the right permutation of the frees in canonical form
-- I.e. (3 2 1 4)[a b c e d d], then we just need the permutation that places the dummies
-- in the correct location.
-- [c b a e d d]
--          2 4
collectContractions' :: Calc -> [[Int]] -> ([Int], Calc)
collectContractions' (Permute p c) cs = collectContractions' c cs
collectContractions' (Contract i1 i2 c) cs = collectContractions' c ([i1, i2]:cs)
collectContractions' c cs = (contractions, c)
          where popOne (ps, conts) [c1, c2] | c2 > c1 = (ps'', n1:n2:conts)
                                                        where (ps', n2) = popAt c2 ps
                                                              (ps'', n1) = popAt c1 ps'
                (_, contractions) = foldl popOne (emptyContractState $ nFreeIndices c) cs