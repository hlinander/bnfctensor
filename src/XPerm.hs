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