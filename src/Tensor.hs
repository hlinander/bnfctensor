module Tensor where

import Data.List

import Frontend.AbsTensor

-- (T.a + S^b.b.a) T^a
usedIndices :: Expr -> [Index]
usedIndices x = case x of
        -- in s, in indices or free
    Tensor _ indices -> indices
    Func label exprs -> undefined
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
    Tensor _ indices -> filter isFree indices
        where isFree index = not (indexLabelIn index s || occurences index indices > 1)
              occurences x list = length $ filter (valenceFreeEq x) list
    Func label exprs -> undefined
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
    Neg expr -> freeIndexSlots_ expr s
    Div expr1 expr2 -> freeIndexSlots expr1
    Number integer -> []
    Fraction integer1 integer2 -> []
    Add expr1 expr2 -> freeIndexSlots_ expr1 s
    Sub expr1 expr2 -> freeIndexSlots_ expr1 s
    Func label exprs -> undefined
    Mul expr1 expr2 -> do
        let leftHand = freeIndexSlots_ expr1 (s ++ freeIndexSlots_ expr2 s)
        let rightHand = freeIndexSlots_ expr2 (s ++ freeIndexSlots_ expr1 s)
        leftHand ++ offsetIndices leftHand rightHand
        -- where offset lh rh = [ (s, i + length lh) | (s,i) <- rh ]

offsetIndices :: [(Index, Int)] -> [(Index, Int)] -> [(Index, Int)]
offsetIndices lh rh = [ (s, i + length lh) | (s,i) <- rh ]

valenceFreeEq :: Index -> Index -> Bool
valenceFreeEq (Upper a) (Lower b) = a == b
valenceFreeEq (Lower a) (Upper b) = a == b
valenceFreeEq (Lower a) (Lower b) = a == b
valenceFreeEq (Upper a) (Upper b) = a == b

indexLabelIn :: Index -> [Index] -> Bool
indexLabelIn index list = any (valenceFreeEq index) list 