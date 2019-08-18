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
execute _ "tree" (c:[]) = renderTreeRepl c
execute _ "sort" (c:[]) = sortCalc c
execute _ "sumsort" (c:[]) = sortSum c
execute _ "elm" (c:[]) = eliminateMetrics c
execute _ "collect" (c:[]) = fixPoint collectTerms c
execute _ "sub" (c:pos) = case subCalc c pos of
                          Just calc -> calc
                          Nothing -> c
execute bs "canon" (c:[]) = canonPerm bs c
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

canonPerm :: BookState -> Calc -> Calc
canonPerm bs c@(Prod p1 p2) = Prod (canonPerm bs p1) (canonPerm bs p2)
canonPerm bs c@(Contract _ _ _) = c
canonPerm bs (Sum s1 s2) = Sum (canonPerm bs s1) (canonPerm bs s2)
canonPerm bs c@(Number _) = c
canonPerm bs c = permToCalc cfree (traceShowId outPerm)
  where frees = [1..(nFreeIndices c)]
        (perm, cfree, gs) = permData bs c
        -- permWithSign = perm ++ [length perm + 1, length perm + 2]
        permWithSign = [1,2] ++ (map ((+) 2) perm)
        outPerm = canonicalizeFree permWithSign (map fromPermutation gs)


permToCalc :: Calc -> [Int] -> Calc
permToCalc c perm = Prod sign (Permute (toPermutation $ traceShow lperm lperm) c)
  where lperm = map (flip (-) 2) $ drop 2 perm
        [s1, s2] = take 2 perm
        sign = if s2 > s1 then Number 1 else Number (-1)

permData :: BookState -> Calc -> ([Int], Calc, [Permutation])
permData bs (Permute p c@(Tensor name _)) = (fromPermutation p, c, vinst)
  where vinst = lookupGeneratingSet $ fromJust $ M.lookup name $ bookTensors bs
permData bs c@(Tensor name _) = ([1..(nFreeIndices c)], c, vinst)
  where vinst = lookupGeneratingSet $ fromJust $ M.lookup name $ bookTensors bs

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
      (commuteContractPermute
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
        --f p@(Prod (Prod p1 p2) (Prod p3 p4))
        --  | (p2 > p3 && (l2 > 0 && l3 > 0)) = Permute perm' $ Prod (Prod p1 p3) (Prod p2 p4)
        --  | (p2 > p3) = Prod (Prod p1 p3) (Prod p2 p4)
        --      where l1 = nFreeIndices p1
        --            l2 = nFreeIndices p2
        --            l3 = nFreeIndices p3
        --            l4 = nFreeIndices p4
        --            perm = concatPermutations (identity l1) $ multiplyMany $ map (\_ -> (cycleLeft (l2 + l3))) [1..l2]
        --            perm' = concatPermutations perm (identity l4)
        -- f p@(Prod (Prod p1 p2) p3)
        --   | (p2 > p3 && (l2 > 0 && l3 > 0)) = Permute perm $ Prod (Prod p1 p3) p2
        --   | (p2 > p3) = Prod (Prod p1 p3) p2
        --       where l1 = nFreeIndices p1
        --             l2 = nFreeIndices p2
        --             l3 = nFreeIndices p3
        --             perm = concatPermutations (identity l1) $ multiplyMany $ map (\_ -> (cycleLeft (l2 + l3))) [1..l2]
        -- f p@(Prod p1 (Prod p2 p3))
        --   | (p1 > p2 && (l1 > 0 && l2 > 0)) = Permute perm $ Prod p2 (Prod p1 p3)
        --   | (p1 > p2) = Prod p2 (Prod p1 p3)
        --       where l1 = nFreeIndices p1
        --             l2 = nFreeIndices p2
        --             l3 = nFreeIndices p3
        --             perm = concatPermutations (multiplyMany $ map (\_ -> (cycleLeft (l1 + l2))) [1..l1]) (identity l3)
        f p@(Prod p1 p2)
          | (p1 > p2 && (l1 > 0 && l2 > 0)) = Permute perm $ Prod p2 p1
          | p1 > p2 = Prod p2 p1
              where l1 = length $ indexFromCalc p1
                    l2 = length $ indexFromCalc p2
                    perm = multiplyMany $ map (\_ -> (cycleLeft (l1 + l2))) [1..l1]
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

flattenProd :: Calc -> [Calc]
flattenProd (Prod s1 s2) = flattenProd s1 ++ flattenProd s2
flattenProd x = [x]

treeProd :: [Calc] -> Calc
treeProd (c:[]) = c
treeProd l = foldl1 Prod l

rewriteProd = fixPoint (transform f)
  where f = treeProd . flattenProd

collectSumList c = case c of
  t1:t2:rest
    | t1 == t2 -> collectSumList (Prod (Number 2) t1 : rest)
  (Prod (Number p) f2):t2:rest
    | f2 == t2 -> collectSumList (Prod (Number (p + 1)) t2 : rest)
  t1:(Prod (Number p) f2):rest
    | t1 == f2 -> collectSumList (Prod (Number (p + 1)) t1 : rest)
  (Prod (Number q) f1):(Prod (Number p) f2):rest
    | f1 == f2 -> collectSumList (Prod (Number (p + q)) f1 : rest)
  t1:t2:rest -> t1:(collectSumList (t2:rest))
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

-- collectTerms2 = transform f
--   where f c@(Sum s1 s2) = concat $ groupBy isSimilar (flattenSum c)

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




commuteContractPermute :: Calc -> Calc
commuteContractPermute = transform commuteContractPermute'

simplifyFactors' :: Calc -> Calc
simplifyFactors' (Prod (Number n) (Number m)) = Number (n*m)
simplifyFactors' (Prod (Number m) (Prod (Number n) f)) = Prod (Number (n*m)) f
simplifyFactors' (Prod (Prod (Number n) f1) f2) = Prod (Number n) (Prod f1 f2)
simplifyFactors' (Prod f1 (Prod (Number n) f2)) = Prod (Number n) (Prod f1 f2) -- TODO: Duplicate rule
simplifyFactors' (Prod (Number 1) f) = f
-- simplifyFactors' (Prod (Number 0) f) = Number 0
simplifyFactors' x = x

simplifyPermutations :: Calc -> Calc
simplifyPermutations = transform simplifyPermutations'

simplifyPermutations' :: Calc -> Calc
simplifyPermutations' (Prod (Permute p f1) f2) = Permute (concatPermutations p $ identity (length $ indexFromCalc f2)) (Prod f1 f2)
simplifyPermutations' (Prod f1 (Permute p f2)) = Permute (concatPermutations (identity (length $ indexFromCalc f1)) p) (Prod f1 f2)
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


-- Contract 1 3 (Permute [0 1 2 3])
-- Contract i1 i2 @ Permute p list = Permute p' @ Contract i1' i2' list
-- p(i1') = i1 => i1' = p^-1(i1)
commuteContractPermute' :: Calc -> Calc
commuteContractPermute' (Contract i1 i2 (Permute perm c)) = if permutationSize newPerm > 0
  then Permute newPerm (Contract i1'' i2'' c)
  else (Contract i1'' i2'' c)
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
  Permute p c -> (permutationSize p == length (indexFromCalc c)) && validCalc c
  Contract i1 i2 c -> inRange i1 0 (n - 1)
                   && inRange i2 0 (n - 1)
                   && (indexRepr (indices !! i1) == indexRepr (indices !! i2))
                   && validCalc c
    where inRange i min max = (i >= min) && (i <= max)
          indices = indexFromCalc c
          n = length indices
  Prod f1 f2 -> validCalc f1 && validCalc f2
  Sum t1 t2 -> validCalc t1 && validCalc t2 && (sort freeLH) == (sort freeRH)
    where freeLH = indexFromCalc t1
          freeRH = indexFromCalc t2
  Op _ _ c -> validCalc c
  _ -> True

replaceIndex :: Calc -> Int -> Index -> Calc
replaceIndex c i idx = case c of
    Tensor n idxs -> Tensor n (replaceAt i idx idxs)
    Sum s1 s2 -> Sum (replaceIndex s1 i idx) (replaceIndex s2 i idx)
    Prod f1 f2
        | i < n -> Prod (replaceIndex f1 i idx) f2
        | i >= n -> Prod f1 (replaceIndex f2 (i - n) idx)
            where n = length $ indexFromCalc f1
    Contract i1 i2 c -> Contract i1 i2 (replaceIndex c i' idx)
        where i' = indexUnderContract i1 i2 i
    Permute perm c -> Permute perm (replaceIndex c (image perm i) idx)
    Op n idxs c'
        | i < length idxs -> Op n (replaceAt i idx idxs) $ c'
        | i >= length idxs -> Op n idxs $ replaceIndex c' (i - (length idxs)) idx
    _ -> c

-- WHAT DOES THIS FUNCTION DO?
indexUnderContract :: Int -> Int -> Int -> Int
indexUnderContract i1 i2 i
    | i2 > i1 = (deleteAt i1 $ deleteAt i2 [0..]) !! i
    | i2 < i1 = (deleteAt i2 $ deleteAt i1 [0..]) !! i

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

generatingSet :: Calc -> [[Int]]
generatingSet (Tensor "T" _) = [[2, 1, 4, 3]]
generatingSet (Prod c1 c2) = gs1 ++ gs2
  where gl1 = generatingSet c1
        gl2 = generatingSet c2
        n1 = nFreeIndices c1
        n2 = nFreeIndices c2
        gs1 = case gl1 of
                [[]] -> []
                l -> map (fromPermutation . padGenSetRight n2 . prepGenSet) l
        gs2 = case gl2 of
                [[]] -> []
                l -> map (fromPermutation . padGenSetLeft n1 . prepGenSet) l
generatingSet (Contract _ _ c) = generatingSet c
generatingSet (Permute _ c) = generatingSet c
generatingSet _ = [[]]

padGenSetLeft n (p, s) = concatPermutations (concatPermutations (identity n) p) s
padGenSetRight n (p, s) = concatPermutations (concatPermutations p (identity n)) s

prepGenSet gs = (p, s)
  where (l, sl) = splitAt (length gs - 2) gs
        p = toPermutation l
        s = sortingPermutationAsc sl

-- [0 1 2 3 4 5]
--      ^   ^
type Positions = [Int]
type AbsoluteContractions = [Int]
type ContractState = (Positions, AbsoluteContractions)
emptyContractState n = ([0..n], []) :: ContractState

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
                (_, contractions) = foldl (popOne) (emptyContractState $ nFreeIndices c) (traceShowId cs)

colCont :: Calc -> [Int] -> ([Int], [Int], Calc)
colCont c p = colCont' c p []

colCont' :: Calc -> [Int] -> [Int] -> ([Int], [Int], Calc)
colCont' (Contract i1 i2 c) lperm dummies = colCont' c p' (dummies ++ [n1, n2])
          where n2 = nFreeIndices c
                n1 = n2 - 1
                p' = insertAt i2 n2 $ insertAt i1 n1 lperm
colCont' c lperm dummies = (lperm, dummies, c)

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

type PermutationList = [Int]
type DummyNames = [Int]

xPermData :: Calc -> (PermutationList, DummyNames, Calc)
xPermData (Permute p c) = colCont c (fromPermutation p)
xPermData c = colCont c [1..(nFreeIndices c)]

lowerIndices bs = transform (lowerIndices' bs)

lowerIndices' :: BookState -> Calc -> Calc
lowerIndices' bs t@(Tensor l idx) = calc
  where needFix = filter ((==Up) . indexValence . fst) (zip idx [0..])
        calc = unsafeEither $ foldM (switchValence bs) t (map snd needFix)
lowerIndices' bs x = x


--canonicalise c = show lperm ++ show dummies ++ " " ++ (renderConsole c')
--  where (lperm, dummies, c') = colCont c [1..(nFreeIndices c)]

-- canonicaliseFrees term = 