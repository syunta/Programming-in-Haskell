module Chapter7 where

import Data.Char

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

-- 8.
-- Base conversion
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- Transmission
count :: Eq a => a -> [a] -> Int -- added
count t = length . (filter (==t))

parity :: [Bit] -> [Bit] -- added
parity bits | odd . (count 1) $ bits = 1 : bits
            | otherwise              = 0 : bits

unparity :: [Bit] -> [Bit] -- added
unparity bits | head bits == 1 && odd ones  = tail bits
              | head bits == 0 && even ones = tail bits
              | otherwise = error "ParityError"
                where ones = (count 1) . tail $ bits

encode :: String -> [Bit]
encode = concat . map (parity . make8 . int2bin . ord) -- modified

chop9 :: [Bit] -> [[Bit]] -- added
chop9 []   = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int . unparity) . chop9 --modified

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- 9.
noisytransmit :: String -> String
noisytransmit = decode . tail . encode
