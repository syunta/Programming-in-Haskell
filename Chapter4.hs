module Chapter4 where

-- 1.
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
           where
             n = length xs `div` 2

-- 2.
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs | null xs = []
             | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs

-- 3.
(<||>) :: Bool -> Bool -> Bool
False <||> False = False
_     <||> _     = True

(<|||>) :: Bool -> Bool -> Bool
True <|||> _    = True
_    <|||> True = True
_    <|||> _    = False

(<||||>) :: Bool -> Bool -> Bool
x <||||> y = not (not (x && x) && not (y && y)) -- use NAND only

(<|||||>) :: Bool -> Bool -> Bool
x <|||||> y = not (not x && not y) -- use De Morgan's law

-- 4.
(<&&>) :: Bool -> Bool -> Bool
x <&&> y = if x then if y then True else False else False

-- 5.
(<&&&>) :: Bool -> Bool -> Bool
x <&&&> y = if x then y else False

-- 6.
mult :: Num a => a -> a -> a -> a
mult = \x -> (\y -> (\z -> x * y * z))
