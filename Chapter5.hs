module Chapter5 where

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
