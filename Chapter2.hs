module Chapter2 where

-- 1.
{-

(2^3) * 4
(2 * 3) + (4 * 5)
2 + (3 * (4^5))
 
 -}

-- 2.
-- skip

-- 3.
-- 1: N -> n
-- 2: xs のインデント
-- 3: 'div' -> `div`
n :: Int
n = a `div` length xs
    where
      a = 10
      xs = [1, 2, 3, 4, 5]

-- 4.
last' :: [a] -> a
last' xs = head (drop n xs)
           where
             n = length xs - 1

last'' :: [a] -> a
last'' xs = xs !! (length xs - 1)

last''' :: [a] -> a
last''' = head . reverse

-- 5.
init' :: [a] -> [a]
init' xs = take n xs
           where
             n = length xs - 1

init'' :: [a] -> [a]
init'' = reverse . tail . reverse
