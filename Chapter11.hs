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
