module Chapter11 (module Chapter11, choices) where

import CountDown

-- 1.
choices' :: [a] -> [[a]]
choices' xs = concat [perms ys | ys <- subs xs]

-- 2.
remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove t (x:xs) | t == x    = xs
                | otherwise = x : remove t xs

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _      = True
isChoice _  []     = False
isChoice xs (y:ys) = isChoice (remove y xs) ys

-- 3.
{-
split [1,2,3,4]
=> [([],[1,2,3,4]),([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4]),([1,2,3,4],[])]

のような振る舞いを許すと、exprs の

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns,
                 l       <- exprs ls,
                 r       <- exprs rs,
                 e       <- combine l r]

l <- exprs ls の部分で exprs [1,2,3,4] が起こり、無限のループに陥る
-}

-- 4.
countExprs :: [Int] -> Int
countExprs ns = length [e | ns' <- choices ns, e <- exprs ns']
-- ghci> countExprs [1,3,7,10,25,50]
-- 33665406

countSolutions :: [Int] -> Int
countSolutions ns = length [e | ns' <- choices ns, e <- exprs ns', eval e /= []]
-- ghci> countSolutions [1,3,7,10,25,50]
-- 4672540

-- 5.
valid'' :: Op -> Int -> Int -> Bool
valid'' Add _ _ = True
valid'' Sub x y = True
valid'' Mul _ _ = True
valid'' Div x y | y == 0    = False
                | otherwise = x `mod` y == 0

eval' :: Expr -> [Int]
eval' (Val n)     = [n | n > 0]
eval' (App o l r) = [apply o x y | x <- eval' l,
                                   y <- eval' r,
                                   valid'' o x y]

countSolutions' :: [Int] -> Int
countSolutions' ns = length [e | ns' <- choices ns, e <- exprs ns', eval' e /= []]
-- ghci> countSolutions' [1,3,7,10,25,50]
-- 10839369

-- 6.
valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0
-- 1 ^ y = 1
-- x ^ 1 = x
valid' Exp x y = x /= 1 && y /= 1 -- a

apply' :: Op -> Int -> Int -> Int
apply' Add x y = x + y
apply' Sub x y = x - y
apply' Mul x y = x * y
apply' Div x y = x `div` y
apply' Exp x y = x ^ y

results' :: [Int] -> [Result]
results' []  = []
results' [n] = [(Val n,n) | n > 0]
results' ns  = [res | (ls,rs) <- split ns,
                       lx     <- results' ls,
                       ry     <- results' rs,
                       res    <- combine'' lx ry]

combine'' :: Result -> Result -> [Result]
combine'' (l,x) (r,y) = [(App o l r, apply' o x y) | o <- ops', valid' o x y]

ops' :: [Op]
ops' = [Add,Sub,Mul,Div,Exp]

maxmin :: (Ord a, Num a) => a -> [a] -> a -- b
maxmin n = foldl1 (\acc x -> if (abs (n-acc)) < (abs (n-x)) then acc else x)

solutions'' :: [Int] -> Int -> [Result]
solutions'' ns n = filter ((==mx) . snd) rs
                   where
                     rs = [(e,m) | ns' <- choices ns, (e,m) <- results' ns']
                     mx = ((maxmin n) . (map snd)) rs
