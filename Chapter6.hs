module Chapter6 where

import Chapter4

-- 1.
(<^>) :: Integral a => a -> a -> a
_ <^> 0 = 1
x <^> y = x * x <^> (y - 1)

{-

2^3
  = { ^ を適用 }
2 * 2^2
  = { ^ を適用 }
2 * (2 * 2^1)
  = { ^ を適用 }
2 * (2 * (2 * 2^0))
  = { ^ を適用 }
2 * (2 * (2 * 1))
  = { * を適用 }
8

-}

-- 2.

{-

length [1, 2, 3]
  = { length を適用 }
1 + length [2, 3]
  = { length を適用 }
1 + (1 + length [3])
  = { length を適用 }
1 + (1 + (1 + length []))
  = { length を適用 }
1 + (1 + (1 + 0))
  = { + を適用 }
3

drop 3 [1, 2, 3, 4, 5]
  = { drop を適用 }
drop 2 [2, 3, 4, 5]
  = { drop を適用 }
drop 1 [3, 4, 5]
  = { drop を適用 }
drop 0 [4, 5]
  = { drop を適用 }
[4, 5]

init [1, 2, 3]
  = { init を適用 }
1 : init [2, 3]
  = { init を適用 }
1 : (2 : init [3])
  = { init を適用 }
1 : (2 : [])
  = { : を適用 }
[1, 2]

-}

-- 3.
and' :: [Bool] -> Bool
and' []     = True
and' (False:_) = False
and' (_:xs) = and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate'' :: Int -> a -> [a]
replicate'' 0 _ = []
replicate'' n x = x : replicate'' (n - 1) x

(<!!>) :: [a] -> Int -> a
(x:xs) <!!> 0 = x
(x:xs) <!!> n = xs <!!> (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' t (x:xs) = t == x || elem' t xs

-- 4.
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys         = ys
merge xs []         = xs
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

--5.
msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort ys) (msort zs)
            where (ys, zs) = halve xs

--6.
sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 xs     = []
take' n (x:xs) = x : take' (n - 1) xs

last'''' :: [a] -> a
last'''' [x]    = x
last'''' (_:xs) = last'''' xs
