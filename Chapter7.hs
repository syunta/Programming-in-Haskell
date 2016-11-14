module Chapter7 where

-- 1.
mapfilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapfilter f p = (map f) . (filter p)

-- 2.
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (\x acc -> (p x) && acc) True

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\x acc -> (p x) || acc) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p (x:xs) | (p x)     = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p (x:xs) | (p x)     = dropWhile' p xs
                    | otherwise = x:xs

-- 3.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x) : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x:acc else acc) []

-- 4.
-- 1000*a + 100*b + 10*c + 1*d
-- (100*a + 10*b + c) * 10 + d
-- ((10*a + b) * 10 + c) * 10 + d
-- (((1*a) * 10 + b) * 10 + c) * 10 + d
-- (((0 * 10 + a) * 10 + b) * 10 + c) * 10 + d
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0

-- 5.
{-

リストの要素の型が違うから誤りと考えられる

[sum, map (^2), filter even]
[[a] -> a, [a] -> [a], [a] -> [a]]

-}

-- 6.
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x -> (\y -> f (x, y))

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y

-- 7.
type Bit = Int

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = unfold null (f . head) tail xs

iterate' :: (a -> a) -> a -> [a]
iterate' f x = unfold fail id f x
               where fail _ = False