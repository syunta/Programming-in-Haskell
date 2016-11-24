module Chapter10 where

import Chapter4 (halve)
import Chapter9 (rmdups)

data Nat = Zero | Succ Nat
           deriving (Show, Eq)

-- 1.
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult m (Succ n) = add m (mult m n)

-- 2.
data Tree = Leaf Int | Node Tree Int Tree
            deriving (Show, Eq)

-- 比較が一回で済むため効率が良い
occurs :: Int -> Tree -> Bool
occurs m (Leaf n)     = m == n
occurs m (Node l n r) = case compare m n of
                          EQ -> True
                          LT -> occurs m l
                          GT -> occurs m r

-- 3.
countleaves :: Tree -> Int
countleaves (Leaf n) = 1
countleaves (Node l _ r) = countleaves l + countleaves r

balanced :: Tree -> Bool
balanced (Leaf n) = False
balanced (Node l _ r) = abs (countleaves l - countleaves r) <= 1

-- 4.
balance :: [Int] -> Tree
balance xs  = case halve xs of
                ([y],[])    -> Leaf y
                ([],[y])    -> Leaf y
                (ys,(z:zs)) -> Node (balance ys) z (balance zs)

-- 5.

-- Propositions

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | IFF Prop Prop
          | Imply Prop Prop

-- Substitutions

type Subst = Assoc Char Bool

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

-- Tautology checker

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (IFF p q)   = eval s p == eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
