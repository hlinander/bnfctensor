module Transform where

import Test.QuickCheck

import Math.Combinat.Permutations

import Data.Generics.Uniplate.Direct
import Data.Maybe
import Data.List
import Data.Ratio
import Control.Monad.Reader
import qualified Data.Map as M

import Debug.Trace
-- import Debug
import System.IO.Unsafe
-- import XPerm

import Core
import RenderCalc
import Util
import RustPerm
import Foreign.Ptr


execute :: BookState -> String -> [Calc] -> Calc
execute _ "distribute" (c:[]) = distribute c
execute _ "leibnitz" (c:[]) = leibnitz c
execute _ "simp" (c:[]) = simplify c
execute _ "show" (c:[]) = showCalc c
execute _ "ccp" (c:[]) = commuteContractPermute c
execute _ "cpc" (c:[]) = commutePermuteContract c
execute _ "tree" (c:[]) = renderTreeRepl c
execute _ "sort" (c:[]) = sortCalc c
execute _ "sumsort" (c:[]) = sortSum c
execute _ "elm" (c:[]) = eliminateMetrics c
execute _ "collect" (c:[]) = fixPoint collectTerms c
execute _ "sub" (c:pos) = case subCalc c pos of
                          Just calc -> calc
                          Nothing -> c
execute bs "canon" (c:[]) = canonTerms bs c
execute _ _ (c:rest) = c

fixPoint :: (Calc -> Calc) -> Calc -> Calc
fixPoint = fixPoint' 100

fixPoint' :: Int -> (Calc -> Calc) -> Calc -> Calc
fixPoint' n f c = case n of
    0 -> c
    n | c' == c -> c
      | otherwise -> fixPoint' (n-1) f c'
    where c' = f c

showCalc :: Calc -> Calc
showCalc c = traceShowId c


flattenProduct :: Calc -> [Calc]
flattenProduct (Prod s1 s2) = flattenProduct s1 ++ flattenProduct s2
flattenProduct x = [x]

canonTerms :: BookState -> Calc -> Calc
canonTerms bs c = runReader (canonTerms' c) $ emptyCanonEnv bs


emptyCanonEnv = C []

-- type Dummy = (Int, Int)
data CanonEnv = C
  { relativeDummies :: [RelDummy]
  , bookState :: BookState
  }

-- GS [1 0 2]+ [0 2 1]+
-- C 0 1 [a b c] + C 1 2 [a b c]
-- Choose a representation with [frees dummies]
-- Insert P^-1 P for the permutation that moves the dummies to the end
-- Canonicalise leaving the inverse on the outside
-- 
-- C 0 1 P [2 0 1] (P [1 2 0] [a b c]) + C 1 2 [a b c]

-- GS [1 0 2]+ [0 2 1]+
-- C 0 1 [c b a] + C 1 2 [a b c]
-- C 0 1 P [2 1 0] [a b c] + C 1 2 [a b c]
-- Choose a representation with [frees dummies]
-- Insert P^-1 P for the permutation that moves the dummies to the end
-- Canonicalise leaving the inverse on the outside
-- 
-- C 0 1 P [2 0 1] (P [1 2 0] P [2 1 0] [a b c]) + C 1 2 [a b c]

-- Then need to derive generating set for dummies acting on the base indices to be
-- used by Knuth.
-- GSd P x = P GSd' x
-- GSd' = P^-1 GSd P
-- Need GS for dummies acting on base slots
-- An element of GSd (acting on permuted indices) can be brought to act on the base indices

-- collectDummies :: Calc -> ([Int], [Dummy])
-- collectDummies c = case c of
--   (Number _) -> ([], [])
--   (Power _ _) -> ([], [])
--   (Tensor _ []) -> ([], [])
--   (Tensor _ idx) -> ([0..length idx - 1], [])
--   (Prod f1 f2) -> (frees1 ++ offsetSequence n1 frees2, dummies1 ++ offsetTupleSequence n1 dummies2)
--     where (frees1, dummies1) = collectDummies f1
--           (frees2, dummies2) = collectDummies f2
--           n1 = length frees1 + 2 * length dummies1
--   (Contract i1 i2 c)
--       | i1 < i2 -> (deleteAt i1 $ deleteAt i2 frees, (frees !! i1, frees !! i2) : dummies)
--       | i1 > i2 -> (deleteAt i2 $ deleteAt i1 frees, (frees !! i1, frees !! i2) : dummies)
--     where (frees, dummies) = collectDummies c
--   _ -> undefined

canonTerms' :: Calc -> Reader CanonEnv Calc 
canonTerms' c = case c of
  Sum  s1 s2 -> Sum <$> (canonTerms' s1) <*> (canonTerms' s2)
  Contract i1 i2 c' -> local (appendDummy (i1, i2)) (canonTerms' c')
  Permute p c' -> canonTerm p c'
  c' -> canonTerm (identity $ length $ allIndices c') c'
  _ -> return c
  where appendDummy d = (\e -> e { relativeDummies = d : relativeDummies e }) 

canonTerm :: Permutation -> Calc -> Reader CanonEnv Calc
canonTerm p c = do
  C rd bs <- ask
  let tensors = flattenProduct c
      tt (Tensor name _) = fromJust $ lookupTensor name bs
      gs = termGeneratingSet $ map tt tensors
      gds = [] -- dummyGSWithSign rd p
      sortDummyPerm = traceShowId $ sortDummyPermutation rd (permutationSize p)
      addSign = concatPermutations (identity 2) -- [1,2] ++ map (2 +) (fromPermutation p)
      totalGS = gs ++ gds
      preCanonPerm = fromPermutation $ addSign $ p `multiply` sortDummyPerm
      outPerm = toPermutation $ canonicalizeFree preCanonPerm (map fromPermutation totalGS) 

      -- [(1,2), (3,4)] => [[2 1 3 4], [1 2 4 3], [3 4 1 2]]
      -- dummies = absoluteDummies rd (permutationSize p)
  return $ contractTerm rd $ permuteTerm c (outPerm `multiply` (addSign $ inverse sortDummyPerm))

contractTerm :: [RelDummy] -> Calc -> Calc
contractTerm rds c = foldr addContract c (reverse rds)
  where addContract (i1, i2) c = Contract i1 i2 c

permuteTerm :: Calc -> Permutation -> Calc
permuteTerm c perm = Prod sign (Permute (toPermutation idxperm) c)
  where lPerm = fromPermutation perm
        idxperm = map (flip (-) 2) $ drop 2 lPerm
        [s1, s2] = take 2 lPerm
        sign = if s2 > s1 then Number 1 else Number (-1)

-- canonPerm :: BookState -> Calc -> Calc
-- canonPerm bs c@(Prod p1 p2) = Prod (canonPerm bs p1) (canonPerm bs p2)
-- canonPerm bs c@(Contract _ _ _) = c
-- canonPerm bs (Sum s1 s2) = Sum (canonPerm bs s1) (canonPerm bs s2)
-- canonPerm bs c@(Number _) = c
-- canonPerm bs c = permToCalc cfree outPerm
--   where (perm, cfree, gs) = permData bs c
--         permWithSign = [1,2] ++ map (2 +) perm
--         outPerm = canonicalizeFree permWithSign (map fromPermutation gs)

--            InnerCalc Pi'    CanonCalc

--                                Pi     InnerCalc   GS
-- permData :: BookState -> Calc -> ([Int], Calc, [Permutation])
-- permData bs (Permute p c@(Tensor name _)) = (fromPermutation p, c, gs)
--   where gs = lookupGeneratingSet $ fromJust $ M.lookup name $ bookTensors bs
-- permData bs c@(Tensor name _) = ([1..(nFreeIndices c)], c, gs)
--   where gs = lookupGeneratingSet $ fromJust $ M.lookup name $ bookTensors bs
-- permData _ _ = undefined

-----------------------------------------------------------------------
-- Basic algebraic convenience
-----------------------------------------------------------------------

distribute :: Calc -> Calc
distribute = fixPoint distribute'

-- first order distribute
distribute' :: Calc -> Calc
distribute' c = case c of
    Prod (Sum s1 s2) fs -> Sum (Prod s1 fs) (Prod s2 fs)
    Prod fs (Sum s1 s2) -> Sum (Prod fs s1) (Prod fs s2)
    Prod f1 f2 -> Prod (distribute' f1) (distribute' f2)
    Sum s1 s2 -> Sum (distribute' s1) (distribute' s2)
    Permute p (Sum s1 s2) -> Sum (Permute p s1) (Permute p s2)
    Permute p c -> Permute p (distribute' c)
    Contract i1 i2 c -> Contract i1 i2 (distribute' c)
    _ -> c

simplify = fixPoint
      (
      commuteContractPermute
    . (fixPoint collectTerms)
    . simplifyContract
    . eliminateMetrics
    . simplifyPermutations
    . simplifyTerms
    . simplifyFactors
    . sortCalc)

sortCalc :: Calc -> Calc
sortCalc = transform f
  where f s@(Sum s1 s2)
          | s1 <= s2 = s
          | otherwise = Sum s2 s1
        f p@(Prod p1 p2)
          | p1 > p2 && (l1 > 0 && l2 > 0) = Permute perm $ Prod p2 p1
          | p1 > p2 = Prod p2 p1
              where l1 = length $ freeIndexFromCalc p1
                    l2 = length $ freeIndexFromCalc p2
                    perm = multiplyMany $ map (const (cycleLeft (l1 + l2))) [1..l1]
        -- Could it be render that is wrong and not sort?
        f x = x

flattenSum :: Calc -> [Calc]
flattenSum (Sum s1 s2) = flattenSum s1 ++ flattenSum s2
flattenSum x = [x]

treeSum :: [Calc] -> Calc
treeSum (c:[]) = c
treeSum l = foldl1 Sum l

sortSum :: Calc -> Calc
sortSum = transform (treeSum . sort . flattenSum)

collectSumList c = case c of
  t1:t2:rest
    | t1 == t2 -> collectSumList (Prod (Number 2) t1 : rest)
  Prod (Number p) f2 : (t2 : rest)
    | f2 == t2 -> collectSumList (Prod (Number (p + 1)) t2 : rest)
  t1 : Prod (Number p) f2 : rest
    | t1 == f2 -> collectSumList (Prod (Number (p + 1)) t1 : rest)
  Prod (Number q) f1 : (Prod (Number p) f2 : rest)
    | f1 == f2 -> collectSumList (Prod (Number (p + q)) f1 : rest)
  t1:t2:rest -> t1 : collectSumList (t2:rest)
  _ -> c

isSimilar (Prod (Number _) c1) c2 | c1 == c2 = True
isSimilar c1 (Prod (Number _) c2) | c1 == c2 = True
isSimilar c1 c2 | c1 == c2 = True
isSimilar _ _ = False

-- turbo haxx, please look away
isSimilar' :: Calc -> Calc -> Ordering
isSimilar' (Prod (Number _) c1) c2 | c1 == c2 = EQ
isSimilar' c1 (Prod (Number _) c2) | c1 == c2 = EQ
isSimilar' c1 c2 | c1 == c2 = EQ
isSimilar' _ _ = GT

collectTerms :: Calc -> Calc
collectTerms = transform collectTerms'

collectTerms' :: Calc -> Calc
collectTerms' c@(Sum s1 s2) = treeSum $ (collectSumList . concat . (groupBy isSimilar) . (sortBy isSimilar') . flattenSum) c
collectTerms' x = x

isCalcSum (Sum _ _) = True
isCalcSum _ = False

prop_flattenTreeSum :: Calc -> Property
prop_flattenTreeSum c = isCalcSum c ==> flattenSum c == (flattenSum $ treeSum $ flattenSum c)

simplifyFactors :: Calc -> Calc
simplifyFactors = transform simplifyFactors'

simplifyTerms :: Calc -> Calc
simplifyTerms = transform sT
  where sT (Sum (Number n) (Number m)) = Number (n+m)
        sT (Sum (Number m) (Sum (Number n) f)) = Sum (Number (n+m)) f
        sT (Sum (Sum (Number n) f1) f2) = Sum (Number n) (Sum f1 f2)
        sT x = x





simplifyFactors' :: Calc -> Calc
simplifyFactors' (Prod (Number n) (Number m)) = Number (n*m)
simplifyFactors' (Prod (Number m) (Prod (Number n) f)) = Prod (Number (n*m)) f
simplifyFactors' (Prod (Prod (Number n) f1) f2) = Prod (Number n) (Prod f1 f2)
simplifyFactors' (Prod f1 (Prod (Number n) f2)) = Prod (Number n) (Prod f1 f2) -- TODO: Duplicate rule
simplifyFactors' (Prod (Number 1) f) = f
simplifyFactors' (Prod (Number 0) f) = Tensor "∅" (freeIndexFromCalc f)
simplifyFactors' (Prod (Number n) (Tensor "∅" idx)) = Tensor "∅" idx
simplifyFactors' c@(Prod (Tensor "∅" idx) f) = Tensor "∅" (freeIndexFromCalc c)
simplifyFactors' c@(Prod f (Tensor "∅" idx)) = Tensor "∅" (freeIndexFromCalc c)
simplifyFactors' x = x

simplifyPermutations :: Calc -> Calc
simplifyPermutations = transform simplifyPermutations'

simplifyPermutations' :: Calc -> Calc
simplifyPermutations' (Prod (Permute p f1) f2) = Permute (concatPermutations p $ identity (length $ freeIndexFromCalc f2)) (Prod f1 f2)
simplifyPermutations' (Prod f1 (Permute p f2)) = Permute (concatPermutations (identity (length $ freeIndexFromCalc f1)) p) (Prod f1 f2)
simplifyPermutations' (Permute p (Prod (Number n) f)) = Prod (Number n) (Permute p f)
simplifyPermutations' (Permute p (Sum t1 t2)) = Sum (Permute p t1) (Permute p t2)
simplifyPermutations' (Permute p (Permute q c)) = Permute (multiply p q) c
simplifyPermutations' (Permute p c) | isIdentityPermutation p = c
simplifyPermutations' x = x

simplifyContract :: Calc -> Calc
simplifyContract = transform simplifyContract'

simplifyContract' :: Calc -> Calc
simplifyContract' (Contract i1 i2 (Sum t1 t2)) = Sum (Contract i1 i2 t1) (Contract i1 i2 t2)
simplifyContract' (Contract i1 i2 (Prod (Number n) f)) = Prod (Number n) (Contract i1 i2 f)
simplifyContract' (Prod (Contract i1 i2 c) f2) = Contract i1 i2 (Prod c f2)
simplifyContract' (Prod f1 (Contract i1 i2 c)) = Contract (i1 + n) (i2 + n) (Prod f1 c)
  where n = nFreeIndices f1
--simplifyContract' (Contract _ _ (Number 0)) = Number 0
simplifyContract' x = x

-- g.[a d]T^[a b c] = T.d^[b c]
--    ^      ^
-- Contract 0 2 => removeMetric 0 (replaceIndex 2 .d)

-- g.[b d]T^[a b c] = T^a.d^c
--    ^        ^
-- id_1 + (2 1)
-- Contract 0 2 => Permute [1 3 2] $ removeMetric 0 (replaceIndex 3 .d)
-- Implement the permutation by performing the same modifications in a numbered
-- list and reading of the permutation as the sorting to canonical ordering.
eliminateMetrics :: Calc -> Calc
eliminateMetrics = fixPoint (transform f)
  where f calc = case calc of
            Contract i1 i2 c
            -- TODO: Replace with fromMaybe $ fmap curry ?
              | i1 < i2 -> case indexOnMetric c i1 of
                            Nothing -> case indexOnMetric c i2 of
                              Nothing -> calc
                              Just (index, oim) -> eliminateOneMetric c i2 oim i1 index
                            Just (index, oim) -> eliminateOneMetric c i1 oim i2 index
            x -> x

eliminateOneMetric c im oim it index = Permute perm $ removeMetric (replaceIndex c it index) im
  where perm = inverse $ sortingPermutationAsc posList
        [r1, r2] = sort [im, oim]
        posList = deleteAt r1 $ deleteAt r2 $ replaceAt it (oim) [0..(nFreeIndices c - 1)]

removeMetric :: Calc -> Int -> Calc
removeMetric c i = case c of
  Tensor "g" idx -> Number 1
  Prod f1 f2
    | i < n1 -> Prod (removeMetric f1 i) f2
    | otherwise -> Prod f1 (removeMetric f2 (i - n1))
    where n1 = nFreeIndices f1
  Sum _ _ -> c
  Permute perm pc -> c
  Contract i1 i2 cc -> c
  Op l idx oc
    | i < length idx -> c
    | otherwise -> Op l idx (removeMetric oc (i - length idx))
  _ -> c

indexOnMetric :: Calc -> Int -> Maybe (Index, Int)
indexOnMetric c i = runReader (indexOnMetric' c i) i

indexOnMetric' :: Calc -> Int -> Reader Int (Maybe (Index, Int))
indexOnMetric' c i = case c of
  Tensor "g" idx
    | i == 0 -> do
      originalPos <- ask
      return $ Just $ (idx !! 1, originalPos + 1)
    | i == 1 -> do
      originalPos <- ask
      return $ Just $ (idx !! 0, originalPos - 1)
  Prod f1 f2
    | i < n1 -> indexOnMetric' f1 i
    | otherwise -> indexOnMetric' f2 (i - n1)
    where n1 = nFreeIndices f1
  Sum _ _ -> return $ Nothing
  Permute perm pc -> return $ Nothing
  Contract i1 i2 cc -> return $ Nothing
  Op _ idx oc
    | i < length idx -> return $ Nothing
    | otherwise -> indexOnMetric' oc (i - length idx)
  _ -> return $ Nothing


commuteContractPermute :: Calc -> Calc
commuteContractPermute = transform commuteContractPermute'
-- Contract 1 3 (Permute [0 1 2 3])
-- Contract i1 i2 @ Permute p list = Permute p' @ Contract i1' i2' list
-- p(i1') = i1 => i1' = p^-1(i1)
commuteContractPermute' :: Calc -> Calc
commuteContractPermute' (Contract i1 i2 (Permute perm c)) = if permutationSize newPerm > 0
  then Permute newPerm (Contract i1'' i2'' c) else Contract i1'' i2'' c
    where list = [0..permutationSize perm]
          permuted = permuteList (inverse perm) list
          permuted' = if i1 < i2
            then deleteAt i1 (deleteAt i2 permuted)
            else deleteAt i2 (deleteAt i1 permuted)
          newPerm = sortingPermutationAsc permuted'
          i1' = fromJust $ elemIndex i1 (permuteList perm list)
          i2' = fromJust $ elemIndex i2 (permuteList perm list)
          [i1'', i2''] = sort [i1', i2'] :: [Int]
commuteContractPermute' x = x

commutePermuteContract :: Calc -> Calc
commutePermuteContract = transform commutePermuteContract'
-- [2 1 3 4] [1 x 2 3 x 4]
commutePermuteContract' :: Calc -> Calc
commutePermuteContract' (Permute perm (Contract i1 i2 c)) = Contract i1 i2 (Permute (toPermutation (traceShowId fullPermuted)) c)
--commutePermuteContract' perm (i1, i2) = fullPermuted
  where full = [1..permutationSize perm + 2]
        full' = if i1 < i2
          then deleteAt i1 (deleteAt i2 full)
          else deleteAt i2 (deleteAt i1 full)
        permuted = permuteList perm full'
        fullPermuted = if i1 > i2
          then insertAt i1 (i1 + 1) (insertAt i2 (i2 + 1) permuted)
          else insertAt i2 (i2 + 1) (insertAt i1 (i1 + 1) permuted)
commutePermuteContract' x = x

        
-- Contract 5 <-> 6 Pos: []
-- |
-- `- toPermutation [3,4,5,6,7,1,2] Pos: [0]
--    |
--    `- (*) Pos: [0,0]
--       |
--       +- Oi^a̲.b̲.c̲^d̲.e̲ Pos: [0,0,0]
--       |
--       `- g^a̲.b̲ Pos: [0,0,1]
validCalc :: Calc -> Bool
validCalc x = case x of
  Permute p c -> (permutationSize p == length (freeIndexFromCalc c)) && validCalc c
  Contract i1 i2 c -> inRange i1 0 (n - 1)
                   && inRange i2 0 (n - 1)
                   && (indexRepr (indices !! i1) == indexRepr (indices !! i2))
                   && validCalc c
    where inRange i min max = (i >= min) && (i <= max)
          indices = freeIndexFromCalc c
          n = length indices
  Prod f1 f2 -> validCalc f1 && validCalc f2
  Sum t1 t2 -> validCalc t1 && validCalc t2 && sort freeLH == sort freeRH
    where freeLH = freeIndexFromCalc t1
          freeRH = freeIndexFromCalc t2
  Op _ _ c -> validCalc c
  _ -> True

replaceIndex :: Calc -> Int -> Index -> Calc
replaceIndex c i idx = case c of
    Tensor n idxs -> Tensor n (replaceAt i idx idxs)
    Sum s1 s2 -> Sum (replaceIndex s1 i idx) (replaceIndex s2 i idx)
    Prod f1 f2
        | i < n -> Prod (replaceIndex f1 i idx) f2
        | i >= n -> Prod f1 (replaceIndex f2 (i - n) idx)
            where n = length $ freeIndexFromCalc f1
    Contract i1 i2 c -> Contract i1 i2 (replaceIndex c i' idx)
        where i' = indexUnderContract i1 i2 i
    Permute perm c -> Permute perm (replaceIndex c (image perm i) idx)
    Op n idxs c'
        | i < length idxs -> Op n (replaceAt i idx idxs) c'
        | i >= length idxs -> Op n idxs $ replaceIndex c' (i - length idxs) idx
    _ -> c

-- | Free index under a contraction
-- Example: Contract 1 3 [a b c d] (with free indices [a c])
--                        0 1 2 3                      0 1                     
-- c has index position 1 over the contraction and position 2 under.
indexUnderContract :: Int -- ^ First index of contraction
                   -> Int -- ^ Second index of contraction 
                   -> Int -- ^ Free index over the contraction
                   -> Int -- ^ Corresponding free index under the contraction
indexUnderContract i1 i2 i
    | i2 > i1 = deleteAt i1 (deleteAt i2 [0..]) !! i
    | i2 < i1 = deleteAt i2 (deleteAt i1 [0..]) !! i

leibnitz :: Calc -> Calc
leibnitz = fixPoint leibnitz'

leibnitz' :: Calc -> Calc
leibnitz' (Op l idx (Prod f1 f2)) = Prod (Op l idx f1) f2 |+| Prod f1 (Op l idx f2)
leibnitz' (Op l idx c) = Op l idx (leibnitz c)
leibnitz' (Sum s1 s2) = Sum (leibnitz s1) (leibnitz s2)
leibnitz' (Permute p c) = Permute p (leibnitz c)
leibnitz' (Contract i1 i2 c) = Contract i1 i2 (leibnitz c)
leibnitz' c = c

subCalc :: Calc -> [Calc] -> Maybe Calc
subCalc c pos = subCalc' c (map numberToInt pos)
  where numberToInt (Number n) | denominator n == 1 = fromIntegral $ numerator n

subCalc' :: Calc -> [Int] -> Maybe Calc
subCalc' c [] = Just c
subCalc' (Sum c1 c2) (0:rest) = subCalc' c1 rest
subCalc' (Sum c1 c2) (1:rest) = subCalc' c2 rest
subCalc' (Prod c1 c2) (0:rest) = subCalc' c1 rest
subCalc' (Prod c1 c2) (1:rest) = subCalc' c2 rest
subCalc' (Permute _ c) (0:rest) = subCalc' c rest
subCalc' (Contract _ _ c) (0:rest) = subCalc' c rest
subCalc' _ _ = Nothing

padGenSetLeft n (p, s) = concatPermutations (concatPermutations (identity n) p) s
padGenSetRight n (p, s) = concatPermutations (concatPermutations p (identity n)) s

prepGenSet gs = (p, s)
  where (l, sl) = splitAt (length gs - 2) gs
        p = toPermutation l
        s = sortingPermutationAsc sl
