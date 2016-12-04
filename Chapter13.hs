module Chapter13 where

-- 1.
{-

null :: [a] -> Bool
null []    = True
null (_:_) = False

last :: [a] -> a
last (_:xs) = last xs
last [x]    = x

(!!) :: [a] -> Int -> a
(x:_)  !! 0       = x
(x:xs) !! (n + 1) = xs !! n

... etc

これらはパターンの順番に依存しているので、重複のあるパターンである。

-}

-- 2.

{-

add :: Nat -> Nat -> Nat
add Zero m     = m
add (Succ n) m = Succ (add n m)

add n (Succ m) = Succ (add n m) をnに対する数学的帰納法で示す

n = Zero のとき :

{左辺} = add Zero (Succ m)
       = { add を適用 }
       = (Succ m)

{右辺} = Succ (add Zero m)
       = { add を適用 }
       = (Succ m)

n = x のとき、以下が成り立つと仮定する :

add x (Succ m) = Succ (add x m)

x = (Succ n) のとき :

{左辺} = add (Succ n) (Succ m)
       = { add を適用 }
       = Succ (add n (Succ m))
       = { 仮定より }
       = Succ (Succ (add n m))

{右辺} = Succ (add (Succ n) m)
       = { add を適用 }
       = Succ (Succ (add n m))
-}

-- 3.

{-

add n m = add m n (交換法則) をnに対する数学的帰納法で示す

n = Zero のとき :

{左辺} = add Zero m
       = { add を適用 }
       = m

{右辺} = add m Zero
       = { add n Zero = n の性質より }
       = m

n = x のとき、以下が成り立つと仮定する :

add x m = add m x

x = (Succ n) のとき :

{左辺} = add (Succ n) m
       = { add を適用 }
       = Succ (add n m)
       = { 仮定より }
       = Succ (add m n)

{右辺} = add m (Succ n)
       = { add n (Succ m) = Succ (add n m) の性質より }
       = Succ (add m n)
-}

-- 4.
{-

all :: Foldable t => (a -> Bool) -> t a -> Bool
all p []     = True
all p (x:xs) = p x && all p xs

all (== x) (repliate n x) = True が常に成り立つことを0以上の整数nに対する数学的帰納法で示す

n = 0 のとき :

{左辺} = all (== x) (repliate 0 x)
       = { repliate を適用 }
       = all (== x) []
       = { all を適用 }
       = True

n = m のとき、以下が成り立つと仮定する :

all (== x) (repliate m x) = True

m = (n + 1) のとき :

{左辺} = all (== x) (repliate (n + 1) x)
       = { all を適用 }
       = all (== x) (x:xs) = (== x) x && all (== x) xs
       = { xs は (replicate n x) なので }
       = (== x) x && all (== x) (repliate n x)
       = { 仮定より }
       = (== x) x && True
       = { (== x), && を適用 }
       = True

-}

-- 5.
{-

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

  1. xs ++ [] = xs
  2. xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

の2つが成り立つことをxsに対する数学的帰納法で示す

1.

xs = [] のとき :

{左辺} = [] ++ []
       = { ++ を適用 }
       = []

{右辺} = []

xs = ns のとき、以下が成り立つと仮定する :

ns ++ [] = ns

ns = (x:xs) のとき :

{左辺} = (x:xs) ++ []
       = { ++ を適用 }
       = x : (xs ++ [])
       = { 仮定より }
       = x : xs

{右辺} = (x:xs)

1. についての証明終了

2.

xs = [] のとき :

{左辺} = [] ++ (ys ++ zs)
       = { ++ を適用 }
       = ys ++ zs

{右辺} = ([] ++ ys) ++ zs
       = { ++ を適用 }
       = ys ++ zs

xs = ns のとき、以下が成り立つと仮定する :

ns ++ (ys ++ zs) = (ns ++ ys) ++ zs

ns = (x:xs) のとき :

{左辺} = (x:xs) ++ (ys ++ zs)
       = { 外の ++ を適用 }
       = x : (xs ++ (ys ++ zs))
       = { 仮定より }
       = x : ((xs ++ ys) ++ zs)
       = { 後述の性質3.より }
       = (x : (xs ++ ys)) ++ zs

{右辺} = ((x:xs) ++ ys) ++ zs
       = { 外の ++ を適用 }
       = (x : (xs ++ ys)) ++ zs

2. の証明終了


3.

(x:xs) ++ ys = x : (xs ++ ys)

xs = [] のとき、

{左辺} = [x] ++ ys
       = { ++ を適用 }
       = x : ([] ++ ys)

{右辺} = x : ([] ++ ys)
       = { ++ を逆適用 }
       = [x] ++ ys

xs = ns のとき、以下が成り立つと仮定する :

(x:ns) ++ ys = x : (ns ++ ys)

ns = (z:zs) のとき :

{左辺} = (x:z:zs) ++ ys
       = { ++ を適用 }
       = x : ((z:zs) ++ ys)

{右辺} = x : ((z:zs) ++ ys)
       = { ++ を逆適用 }
       = (x:z:zs) ++ ys

3. の証明終了

-}
