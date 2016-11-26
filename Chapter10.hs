module Chapter10 (module Chapter10, module Parsing) where

import Chapter4 (halve)
import Chapter9 (rmdups, readLine)
import Parsing
import Data.Char

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

data Prop = Cont Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | IFF Prop Prop
          | Imply Prop Prop
          deriving Show

-- Substitutions

type Subst = Assoc Char Bool

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

-- Tautology checker

eval :: Subst -> Prop -> Bool
eval _ (Cont b)    = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (IFF p q)   = eval s p == eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Cont _)    = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (IFF p q)   = vars p ++ vars q
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

-- 6.
{-

prop   ::= term ('^' term | '=>' term | 'v' term | '==' term | ε)
term   ::= '¬' factor | factor
factor ::= '(' prop ')' | var
var    ::= 'A' | 'B'

 -}

prop :: Parser Prop
prop = do t1 <- term
          do symbol "^"
             t2 <- term
             return (And t1 t2)
           <|>
           do symbol "=>"
              t2 <- term
              return (Imply t1 t2)
           <|>
           do symbol "v"
              t2 <- term
              return (Or t1 t2)
           <|>
           do symbol "=="
              t2 <- term
              return (IFF t1 t2)
           <|>
           return t1

term :: Parser Prop
term = do symbol "¬"
          f <- factor
          return (Not f)
        <|>
        do f <- factor
           return f

factor :: Parser Prop
factor = do symbol "("
            p <- prop
            symbol ")"
            return p
          <|>
          do c <- var
             return (Var c)

var :: Parser Char
var = (char 'A') <|> (char 'B')

runTaut :: IO ()
runTaut = do xs <- readLine
             case parse prop xs of
               [(p,[])]  -> do let b = isTaut p
                               putStrLn (show b)
                               return ()
               [(_,out)] -> do putStrLn "Unused unput, try again"
                               runTaut
               []        -> do putStrLn "Invalid input, try again"
                               runTaut

-- 7.
data Expr = Val Int | Add Expr Expr | Mul Expr Expr

type Cont = [Op]

data Op = EVALA Expr | ADD Int | EVALM Expr | MUL Int

eval' :: Expr -> Cont -> Int
eval' (Val n) c   = exec c n
eval' (Mul x y) c = eval' x (EVALM y : c)
eval' (Add x y) c = eval' x (EVALA y : c)

exec :: Cont -> Int -> Int
exec [] n           = n
exec (EVALM y : c) n = eval' y (MUL n : c)
exec (EVALA y : c) n = eval' y (ADD n : c)
exec (MUL n : c) m  = exec c (n * m)
exec (ADD n : c) m  = exec c (n + m)

value :: Expr -> Int
value e = eval' e []

-- 8.
-- insead of Monad Maybe
data Haybe a = Hothing | Hust a
               deriving (Show, Eq)

instance Functor Haybe where
   -- fmap :: (a -> b) -> Haybe a -> Haybe b
   fmap _ Hothing = Hothing
   fmap f (Hust x) = Hust (f x)

instance Applicative Haybe where
   -- pure :: a -> Haybe a
   pure x = Hust x
   -- <*> :: Haybe (a -> b) -> Haybe a -> Haybe b
   _      <*> Hothing = Hothing
   Hust f <*> Hust x  = Hust (f x)

instance Monad Haybe where
  -- return :: a -> Haybe a
  return x = Hust x
  -- (>>=) :: Haybe a -> (a -> HayBe b) -> Haybe b
  Hothing  >>= _ = Hothing
  (Hust x) >>= f = f x

hothing :: Int -> Haybe Int
hothing x = do Hothing
               Hust x

triple :: Int -> Haybe Int
triple x = do y <- Hust (x + x)
              z <- Hust (y + x)
              return z

-- insead of Monad []
data Cell a = Nil | Cell a (Cell a)
              deriving (Show, Eq)

instance Functor Cell where
   -- fmap :: (a -> b) -> Cell a -> Cell b
   fmap _ Nil = Nil
   fmap f (Cell x xs) = Cell (f x) (fmap f xs)

append :: Cell a -> Cell a -> Cell a
append Nil ys = ys
append (Cell x xs) ys = Cell x (append xs ys)

instance Applicative Cell where
   -- pure :: a -> Cell a
   pure x = Cell x Nil
   -- <*> :: Cell (a -> b) -> Cell a -> Cell b
   _         <*> Nil = Nil
   Nil       <*> _   = Nil
   Cell f fs <*> xs  = fmap f xs `append` (fs <*> xs)

instance Monad Cell where
  -- return :: a -> Cell a
  return x = Cell x Nil
  -- (>>=) :: Cell a -> (a -> Cell b) -> Cell b
  Nil         >>= _ = Nil
  (Cell x xs) >>= f = (f x) `append` (xs >>= f)

--instance Monad [] where
--  return :: a -> [a]
--  return x = [x]
--
--  (>>=) :: [a] -> (a -> [b]) -> [b]
--  []     >>= _ = []
--  (x:xs) >>= f = (f x) ++ (xs >>= f)

nil :: Int -> Cell Int
nil x = do return x
           Nil
           return x

quantaple :: Int -> Cell Int
quantaple w = do x <- Cell (w + w) Nil
                 y <- Cell (x + w) Nil
                 z <- Cell (y + w) Nil
                 return z
