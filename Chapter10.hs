module Chapter10 where

data Nat = Zero | Succ Nat
           deriving (Show, Eq)

-- 1.
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult' :: Nat -> Nat -> Nat
mult' Zero _ = Zero
mult' _ Zero = Zero
mult' m (Succ n) = add m (mult' m n)

-- 2.
data Tree = Leaf Int | Node Tree Int Tree
            deriving Show

-- 比較が一回で済むため効率が良い
occurs :: Int -> Tree -> Bool
occurs m (Leaf n)     = m == n
occurs m (Node l n r) = case compare m n of
                          EQ -> True
                          LT -> occurs m l
                          GT -> occurs m r
