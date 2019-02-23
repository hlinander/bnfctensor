module Tensor where

import Data.List

import Frontend.AbsTensor

-- (T.a + S^b.b.a) T^a
usedIndices :: Expr -> [Index]
usedIndices x = case x of
    Tensor _ indices -> indices
    Anon _ indices -> indices
    Op _ indices expr -> indices ++ (usedIndices expr)
    Func label exprs -> concat (map usedIndices exprs) -- MEGA TODO ONLY WORKS WITH DISTRIBUTE
    Add expr1 expr2 -> union (usedIndices expr1) (usedIndices expr2)
    Sub expr1 expr2 -> union (usedIndices expr1) (usedIndices expr2)
    Neg expr -> usedIndices expr
    Mul expr1 expr2 -> union (usedIndices expr1) (usedIndices expr2)
    Div expr1 expr2 -> usedIndices expr1
    Number integer -> []
    Fraction integer1 integer2 -> []

freeIndices :: Expr -> [Index]
freeIndices x = freeIndices_ x []

freeIndices_ x s = case x of
        -- in s, in indices or free
    Anon _ indices -> filter isFree indices
        where isFree index = not (indexLabelIn index s || occurences index indices > 1)
              occurences x list = length $ filter (valenceFreeEq x) list
    Tensor _ indices -> filter isFree indices
        where isFree index = not (indexLabelIn index s || occurences index indices > 1)
              occurences x list = length $ filter (valenceFreeEq x) list
    Op _ indices expr -> filter isFree allIndices
        where isFree index = not (indexLabelIn index s || occurences index allIndices > 1)
              occurences x list = length $ filter (valenceFreeEq x) list
              allIndices = indices ++ freeIndices_ expr s
    Func label exprs -> []
    Add expr1 expr2 -> freeIndices_ expr1 s
    Sub expr1 expr2 -> freeIndices_ expr1 s
    Neg expr -> freeIndices_ expr s
    Mul expr1 expr2 -> freeIndices_ expr1 (s ++ freeIndices_ expr2 s) ++
                       freeIndices_ expr2 (s ++ freeIndices_ expr1 s)
    Div expr1 expr2 -> freeIndices expr1
    Number integer -> []
    Fraction integer1 integer2 -> []

freeIndexSlots :: Expr -> [(Index, Int)]
freeIndexSlots x = freeIndexSlots_ x []
freeIndexSlots_ x s = case x of
        -- in s, in indices or free
    Tensor _ indices -> zip (filter isFree indices) [0..]
        where isFree index = not (indexLabelIn index (map fst s) || occurences index indices > 1)
              occurences x list = length $ filter (valenceFreeEq x) list
    Anon _ indices -> zip (filter isFree indices) [0..]
        where isFree index = not (indexLabelIn index (map fst s) || occurences index indices > 1)
              occurences x list = length $ filter (valenceFreeEq x) list
    Op _ indices expr -> zip (filter isFree allIndices) [0..]
        where isFree index = not (indexLabelIn index (map fst s) || occurences index allIndices > 1)
              occurences x list = length $ filter (valenceFreeEq x) list
              allIndices = indices ++ freeIndices expr
    Neg expr -> freeIndexSlots_ expr s
    Div expr1 expr2 -> freeIndexSlots expr1
    Number integer -> []
    Fraction integer1 integer2 -> []
    Add expr1 expr2 -> freeIndexSlots_ expr1 s
    Sub expr1 expr2 -> freeIndexSlots_ expr1 s
    Func label exprs -> [] -- MEGA TODO
    Mul expr1 expr2 -> do
        let leftHand = freeIndexSlots_ expr1 (s ++ freeIndexSlots_ expr2 s)
        let rightHand = freeIndexSlots_ expr2 (s ++ freeIndexSlots_ expr1 s)
        leftHand ++ offsetIndices leftHand rightHand
        -- where offset lh rh = [ (s, i + length lh) | (s,i) <- rh ]

freeIndexPos :: Expr -> [(Index, Int)]
freeIndexPos x = freeIndexPos_ x []

freeIndexPos_ :: Expr -> [(Index, Int)] -> [(Index, Int)]
freeIndexPos_ x s = case x of
        -- in s, in indices or free
    Tensor _ indices -> freeIndicesWithPos indices s
    Anon _ indices -> freeIndicesWithPos indices s
    Neg expr -> freeIndexPos_ expr s
    Div expr1 expr2 -> freeIndexPos_ expr1 s
    Number integer -> []
    Fraction integer1 integer2 -> []
    Add expr1 expr2 -> freeIndexPos_ expr1 s
    Sub expr1 expr2 -> freeIndexPos_ expr1 s
    Func label exprs -> [] -- MEGA TODO
    Mul expr1 expr2 -> do
        let leftHand = freeIndexPos_ expr1 (s ++ freeIndexPos_ expr2 s)
        let rightHand = freeIndexPos_ expr2 (s ++ freeIndexPos_ expr1 s)
        leftHand ++ offsetIndices leftHand rightHand
        -- where offset lh rh = [ (s, i + length lh) | (s,i) <- rh ]

freeIndicesWithPos :: [Index] -> [(Index, Int)] -> [(Index, Int)]
freeIndicesWithPos indices used = filter isFree (zip indices [0..])
        where isFree (index, _) = not (indexLabelIn index (map fst used) || occurences index indices > 1)
              occurences x list = length $ filter (valenceFreeEq x) list

offsetIndices :: [(Index, Int)] -> [(Index, Int)] -> [(Index, Int)]
offsetIndices lh rh = [ (s, i + length lh) | (s,i) <- rh ]

valenceFreeEq :: Index -> Index -> Bool
valenceFreeEq (Upper a) (Lower b) = a == b
valenceFreeEq (Lower a) (Upper b) = a == b
valenceFreeEq (Lower a) (Lower b) = a == b
valenceFreeEq (Upper a) (Upper b) = a == b

indexLabelIn :: Index -> [Index] -> Bool
indexLabelIn index list = any (valenceFreeEq index) list 