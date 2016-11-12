module Chapter1 where

-- 1.
{-
 
正規順序の評価

  double (double 2)
= { 外側のdoubleを適用 }
  double 2 + double 2
= { + を適用 }
  double 2 + double 2
= { 一番目のdoubleを適用 }
  (2 + 2) + double 2
= { + を適用 }
  4 + double 2
= { doubleを適用 }
  4 + (2 + 2)
= { + を適用 }
  8
 -}

-- 2.
-- x + 0 = x

-- 3.
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

-- 4.
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b > x]

-- 5.
-- 重複した要素が取り除かれる
qsort' :: Ord a => [a] -> [a]
qsort' []     = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
               where
                 smaller = [a | a <- xs, a < x]
                 larger  = [b | b <- xs, b > x]
