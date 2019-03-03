module Transform where

import Test.QuickCheck

import Math.Combinat.Permutations
import Data.Generics.Uniplate.Direct
import Data.Maybe
import Data.List
import Data.Ratio
import Debug.Trace

import Core
import RenderCalc
import Util


execute :: String -> [Calc] -> Calc
execute "distribute" (c:[]) = distribute c
execute "leibnitz" (c:[]) = leibnitz c
execute "simp" (c:[]) = simplify c
execute "show" (c:[]) = showCalc c
execute "tree" (c:[]) = renderTreeRepl c
execute "sort" (c:[]) = sortCalc c
execute "sumsort" (c:[]) = sortSum c
execute "collect" (c:[]) = fixPoint collectTerms c
execute "elmetrics" (c:[]) = fixPoint eliminateMetrics c
execute "elmetrics2" (c:[]) = fixPoint elMetrics c
execute "sub" (c:pos) = case subCalc c pos of
                          Just calc -> calc
                          Nothing -> c
execute _ (c:rest) = c

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

simplify = fixPoint elMetrics
    . commuteContractPermute
    . simplifyContract
    . simplifyPermutations
    . simplifyTerms
    . simplifyFactors
    . sortCalc

sortCalc :: Calc -> Calc
sortCalc = transform f
  where f s@(Sum s1 s2)
          | s1 <= s2 = s
          | otherwise = Sum s2 s1
        f p@(Prod p1 p2)
          | p1 <= p2 = p
          | (p1 > p2 && (l1 > 0 && l2 > 0)) = Permute perm $ Prod p2 p1
          | otherwise = Prod p2 p1
              where l1 = length $ indexFromCalc p1
                    l2 = length $ indexFromCalc p2
                    perm = multiplyMany $ map (\_ -> (cycleLeft (l1 + l2))) [1..l1]
        f x = x

flattenSum :: Calc -> [Calc]
flattenSum (Sum s1 s2) = flattenSum s1 ++ flattenSum s2
flattenSum x = [x]

treeSum :: [Calc] -> Calc
treeSum (c:[]) = c
treeSum l = foldl1 Sum l

sortSum :: Calc -> Calc
sortSum = treeSum . sort . flattenSum

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

collectTerms :: Calc -> Calc
collectTerms = transform collectTerms'

collectTerms' :: Calc -> Calc
collectTerms' c@(Sum s1 s2) = treeSum $ (collectSumList . sort . flattenSum) c
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


simplifyPermutations :: Calc -> Calc
simplifyPermutations = transform simplifyPermutations'

simplifyContract :: Calc -> Calc
simplifyContract = transform simplifyContract'

commuteContractPermute :: Calc -> Calc
commuteContractPermute = transform commuteContractPermute'

eliminateMetrics :: Calc -> Calc
eliminateMetrics = transform eliminateMetrics'

simplifyFactors' :: Calc -> Calc
simplifyFactors' (Prod (Number n) (Number m)) = Number (n*m)
simplifyFactors' (Prod (Number m) (Prod (Number n) f)) = Prod (Number (n*m)) f
simplifyFactors' (Prod (Prod (Number n) f1) f2) = Prod (Number n) (Prod f1 f2)
simplifyFactors' (Prod f1 (Prod (Number n) f2)) = Prod (Number n) (Prod f1 f2) -- TODO: Duplicate rule
simplifyFactors' (Prod (Number 1) f) = f
simplifyFactors' (Prod (Number 0) f) = Number 0
simplifyFactors' x = x

simplifyPermutations' :: Calc -> Calc
simplifyPermutations' (Prod (Permute p f1) f2) = Permute (concatPermutations p $ identity (length $ indexFromCalc f2)) (Prod f1 f2)
simplifyPermutations' (Prod f1 (Permute p f2)) = Permute (concatPermutations (identity (length $ indexFromCalc f1)) p) (Prod f1 f2)
simplifyPermutations' (Permute p (Prod (Number n) f)) = Prod (Number n) (Permute p f)
simplifyPermutations' (Permute p (Sum t1 t2)) = Sum (Permute p t1) (Permute p t2)
simplifyPermutations' (Permute p (Permute q c)) = Permute (multiply p q) c
simplifyPermutations' x = x

simplifyContract' :: Calc -> Calc
simplifyContract' (Contract i1 i2 (Sum t1 t2)) = Sum (Contract i1 i2 t1) (Contract i1 i2 t2)
simplifyContract' (Contract i1 i2 (Prod (Number n) f)) = Prod (Number n) (Contract i1 i2 f)
simplifyContract' x = x

elMetrics :: Calc -> Calc
elMetrics = transform elMetrics'

elMetrics' :: Calc -> Calc
elMetrics' calc@(Contract i1 i2 c) | i1 < i2 = case indexOnMetric c i1 of
  Nothing -> case indexOnMetric c i2 of
    Nothing -> calc
    Just (index, di) -> case permutationSize perm of
        n | n <= 1 -> removeMetric (replaceIndex c i1 index) i2
        _ -> Permute perm $ removeMetric (replaceIndex c i1 index) i2
      where perm = (id1) `concatPermutations` (cycleRight ncycle) `concatPermutations` id2
            i1' = i1 + (di + 1)
            ncycle = i2 - i1' - 1
            id1 = identity i1'
            id2 = identity (nFreeIndices c - 2 - ncycle - i1')
  Just (index, di) -> case permutationSize perm of
      n | n <= 1 -> removeMetric (replaceIndex c i2 index) i1
      _ -> Permute perm $ removeMetric (replaceIndex c i2 index) i1
    where perm = (id1) `concatPermutations` (cycleLeft ncycle) `concatPermutations` id2
          -- this is wrong depending on if the contraction is on the first or on the last index of the metric
          i1' = i1 + di
          ncycle = i2 - i1' - 1
          id1 = identity i1'
          id2 = identity (nFreeIndices c - 2 - ncycle - i1')
elMetrics' x = x

removeMetric :: Calc -> Int -> Calc
removeMetric c i = case c of
  Tensor "g" idx -> Number 1
  Prod f1 f2
    | i < n1 -> Prod (removeMetric f1 i) f2
    | otherwise -> Prod f1 (removeMetric f2 (i - n1))
    where n1 = nFreeIndices f1
  Sum _ _ -> c
  Permute perm pc -> c -- Permute perm (indexOnMetric pc (image perm i)
  Contract i1 i2 cc -> c -- indexOnMetric cc (indexUnderContract i1 i2 i)
  Op l idx oc
    | i < length idx -> c
    | otherwise -> Op l idx (removeMetric oc (i - length idx))
  _ -> c


indexOnMetric :: Calc -> Int -> Maybe (Index, Int)
indexOnMetric c i = case c of
  Tensor "g" idx -> listToMaybe $ deleteAt i (zip idx [(-1), 0])
  Prod f1 f2
    | i < n1 -> indexOnMetric f1 i
    | otherwise -> indexOnMetric f2 (i - n1)
    where n1 = nFreeIndices f1
  Sum _ _ -> Nothing
  Permute perm pc -> Nothing -- indexOnMetric pc (image perm i)
  Contract i1 i2 cc -> Nothing -- indexOnMetric cc (indexUnderContract i1 i2 i)
  Op _ idx oc
    | i < length idx -> Nothing
    | otherwise -> indexOnMetric oc (i - length idx)
  _ -> Nothing

eliminateMetrics' :: Calc -> Calc
eliminateMetrics' (Contract i1 i2 (Prod (Tensor "g" [ti1, _]) t@(Tensor _ idx)))
  | i2 > 1 && 1 >= i1 = Permute pFix $ setValence t (i2 - 2) (indexValence ti1)
    where pFix = concatPermutations cycle rest
          cycle = cycleLeft $ (i2 - 2) + 1
          rest = identity $ (length idx) - ((i2 - 2) + 1)
eliminateMetrics' (Contract i1 i2 (Prod t@(Tensor _ idx) (Tensor "g" [_, ti])))
  | i2 == (length idx) && i1 < (length idx) = Permute pFix $ setValence t i1 (indexValence ti)
    where pFix = concatPermutations rest cycle
          cycle = cycleLeft $ (length idx) - i1
          rest = identity $ i1
eliminateMetrics' (Contract i1 i2 (Prod t@(Tensor _ idx) (Tensor "g" [ti, _])))
  | i2 == (length idx) + 1 && i1 < (length idx) = Permute pFix $ setValence t i1 (indexValence ti)
    where pFix = concatPermutations rest cycle
          cycle = cycleLeft $ (length idx) - i1
          rest = identity $ i1
eliminateMetrics' (Contract i1 i2 (Prod (Tensor "g" [ti1, _]) t@(Op _ _ _)))
  | i2 > 1 && 1 >= i1 = Permute pFix $ setValence t (i2 - 2) (indexValence ti1)
    where pFix = concatPermutations cycle rest
          cycle = cycleLeft $ (i2 - 2) + 1
          rest = identity $ (length (indexFromCalc t)) - ((i2 - 2) + 1)
eliminateMetrics' x = x

preEliminateMetrics :: Calc -> Calc
preEliminateMetrics = transform (preEliminateMetrics12 . preEliminateMetrics23)

preEliminateMetrics12 (Contract i1 i2 (Prod f1 (Prod f2 f3))) | i1 < n1 && i2 >= n1 && i2 < n1 + n2 =
    Prod (Contract i1 i2 (Prod f1 f2)) f3
  where n1 = length $ indexFromCalc f1
        n2 = length $ indexFromCalc f2
preEliminateMetrics12 x = x

preEliminateMetrics23 (Contract i1 i2 (Prod f1 (Prod f2 f3))) | i1 >= n1 && i1 < n1 + n2 && i2 >= n1 + n2 =
    Prod f1 (Contract (i1 - n1) (i2 - n1) (Prod f2 f3))
  where n1 = length $ indexFromCalc f1
        n2 = length $ indexFromCalc f2
preEliminateMetrics23 x = x

-- collectTerms' :: Calc -> Calc
-- collectTerms' (Sum t1 t2) | t1 == t2 = Prod (Number 2) t1
-- collectTerms' x = x

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

validCalc :: Calc -> Bool
validCalc x = case x of
  Permute p c -> (permutationSize p == length (indexFromCalc c)) && validCalc c
  Contract i1 i2 c -> inRange i1 0 (n - 1)
                   && inRange i2 0 (n - 1)
                   -- && (indexRepr (indices!!i1) == indexRepr (indices!!i2))
                   && validCalc c
    where inRange i min max = (i >= min) && (i <= max)
          indices = indexFromCalc c
          n = length indices
  Prod f1 f2 -> validCalc f1 && validCalc f2
  Sum t1 t2 -> validCalc t1 && validCalc t2
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
        -- | i' < i1 -> Contract i1 i2 (replaceIndex c i idx)
        -- | i' > i1 && i' < i2 -> Contract i1 i2 (replaceIndex c (i+1) idx)
        -- | i' > i2 -> Contract i1 i2 (replaceIndex c (i+2) idx)
        where i' = indexUnderContract i1 i2 i
    Permute perm c -> Permute perm (replaceIndex c (image perm i) idx)
    Op n idxs c'
        | i < length idxs -> Op n (replaceAt i idx idxs) $ c'
        | i >= length idxs -> Op n idxs $ replaceIndex c' (i - (length idxs)) idx
    _ -> c

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