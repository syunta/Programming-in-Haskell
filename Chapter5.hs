module Chapter5 where

import Data.Char

-- 1.
squaresuminhundred :: Int
squaresuminhundred = sum [x^2 | x <- [1..100]]

-- 2.
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

-- 3.
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 4.
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) - x == x]

-- 5.
comprehension_has_two_generaters :: [(Int, Int)]
comprehension_has_two_generaters = [(x, y) | x <- [1,2,3], y <- [4,5,6]]

double_comprehension_has_one_generater :: [(Int, Int)]
double_comprehension_has_one_generater = concat [[(x, y) | y <- [4,5,6]] | x <- [1,2,3]]

-- 6.
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
                 where n = length xs - 1

-- 7.
scalarproduct :: Num a => [a] -> [a] -> a 
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- 8.
-- 大文字の出現頻度も小文字と同じとする

-- Encoding and decoding
lower2int :: Char -> Int -- added
lower2int c = ord c - ord 'a'

int2lower :: Int -> Char -- added
int2lower n = chr (ord 'a' + n)

upper2int :: Char -> Int -- added
upper2int c = ord c - ord 'A'

int2upper :: Int -> Char -- added
int2upper n = chr (ord 'A' + n)

shift :: Int -> Char -> Char -- modified
shift n c | isLower c = int2lower ((lower2int c + n) `mod` 26)
          | isUpper c = int2upper ((upper2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- Frequency analysis

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

letters :: String -> Int -- added
letters xs = length [x | x <- xs, x >= 'a' && x <= 'z' || x >= 'A' && x <= 'Z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float] -- modified
freqs xs = [percent (count x xs + count y xs) n | (x, y) <- zip ['a'..'z'] ['A'..'Z']]
           where n = letters xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
           where
              factor = head (positions (minimum chitab) chitab)
              chitab = [chisqr (rotate n table') table | n <- [0..25]]
              table' = freqs xs
