module Chapter9 where

import Parsing
import Chapter8
import System.IO

type Pos = (Int,Int)

-- 1.
readLine :: IO String
readLine = iter ""
           where
             iter xs = do x <- getChar
                          case x of
                            '\n' -> return (reverse xs)
                            '\DEL' -> do if null xs then
                                           do putStr "\ESC[1D\ESC[1D  \ESC[1D\ESC[1D"
                                              iter xs
                                         else
                                           do putStr "\ESC[1D\ESC[1D\ESC[1D   \ESC[1D\ESC[1D\ESC[1D"
                                              iter (tail xs)
                            _ -> do iter (x:xs)

-- 2.
-- パースエラーの場合、適正な入力位置まで戻るようにした

-- IO utilities
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

writeat :: (Int,Int) -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Calculator

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: String
buttons = standard ++ extra
          where
             standard = "qcd=123+456-789*0()/"
             extra    = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box]

display xs = do writeat (3,2) (replicate 13 ' ')
                writeat (3,2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons then
                 process c xs
             else
                 do beep
                    calc xs

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC"    = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n"       = eval' xs
             | elem c "cC"        = clear
             | otherwise          = press c xs

quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

eval' :: String -> IO ()
eval' xs = case parse expr xs of
             [(n,[])] -> calc (show n)
             [(n,es)] -> do beep -- added
                            calc (show n)
             _        -> do beep
                            calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear

-- 3.

-- Game of life

width :: Int
width = 10

height :: Int
height = 10

type Board = [Pos]

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

clearcells :: Board -> IO () -- added
clearcells b = sequence_ [writeat p " " | p <- b]

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y),
                          (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1, ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO () -- modified
life b = do cls
            go b []
         where go c p = do clearcells p
                           showcells c
                           wait 500000
                           go (nextgen c) c

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]
