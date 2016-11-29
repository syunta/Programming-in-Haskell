module Chapter12 where

-- 1.
{-

1 + (2 * 3)
簡約可能式
  1 + ( ... ) 最も外側
  2 * 3       最も内側

(1 + 2) * (2 + 3)
簡約可能式
  ( ... ) * ( ... ) 最も外側
  (1 + 2)           最も内側
  (2 + 3)           最も内側

fst (1 + 2, 2 + 3)
簡約可能式
  fst ( ... ) 最も外側
  1 + 2       最も内側
  2 + 3       最も内側

(\x -> 1 + x) (2 * 3)
簡約可能式
  (\x ...) ( ... ) 最も外側
  2 * 3            最も内側
  1 + x            いずれでもない

-}

-- 2.

{-

最内簡約だと 1 + 2 と 2 + 3 の両方が評価される。
最外簡約だと fst の 引数は評価されない。
fst の評価で必要なのは 1 + 2 だけなので、最外簡約が適している。

-}

-- 3.

{-

mult 3 4
(\x -> (\y -> x * y) 3 4
(\y -> 3 * y) 4
3 * 4
12

-}

-- 4.
fibs1 :: [Integer]
fibs1 = fibgen 0 1
        where fibgen a b = a : fibgen b (a + b)

fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x,y) <- zip fibs (tail fibs)]

-- 5.
fib :: Int -> Integer
fib n = fibs !! n

getFib :: Integer -> Integer
getFib n = head (dropWhile (<=n) fibs)

-- 6.
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Eq

instance (Show a) => Show (Tree a) where
   show Leaf         = "()"
   show (Node l x r) = "(" ++ (show l) ++ " " ++ (show x) ++ " " ++ (show r) ++ ")"

repeatTree :: a -> Tree a
repeatTree x = tree where tree = Node tree x tree

takeTree :: Int -> Tree a -> Tree a
takeTree 0 _            = Leaf
takeTree n Leaf         = Leaf
takeTree n (Node l x r) = Node (takeTree m l) x (takeTree m r)
                          where m = n - 1

replicateTree :: Int -> a -> Tree a
replicateTree n = (takeTree n) . repeatTree
