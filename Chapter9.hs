module Chapter9 where

import Parsing
import Chapter8
import System.IO
import Data.List (union, deleteBy, splitAt)
import Data.Char

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

-- 4.
editorusage :: [String]
editorusage = ["h: left, j: down, k: up, l: right, q: exit",
               "enter: put creature, del: remove creature"]

showeditor :: Board -> IO ()
showeditor b = do sequence_ [writeat (1,y) b | (y,b) <- zip [1..] (frame ++ editorusage)]
                  showcells b
               where
                 frame = replicate height (replicate width ' ' ++ "*") ++ [replicate (width + 1) '*']

editboard :: Pos -> Board -> IO Board
editboard (x,y) b = do goto (x,y)
                       ch <- getCh
                       case ch of
                         'h' -> editboard (wrap (x-1,y)) b
                         'j' -> editboard (wrap (x,y+1)) b
                         'k' -> editboard (wrap (x,y-1)) b
                         'l' -> editboard (wrap (x+1,y)) b
                         '\n' -> do let b' = [(x,y)] `union` b
                                    showeditor b'
                                    editboard (x,y) b'
                         '\DEL' -> do let b' = deleteBy (==) (x,y) b
                                      showeditor b'
                                      editboard (x,y) b'
                         'q' -> do goto (1,width + 4)
                                   return b
                         _ -> do beep
                                 editboard (x,y) b

runboardeditor :: Board -> IO Board
runboardeditor b = do cls
                      showeditor b
                      editboard (1,1) b

-- 5.
-- skip

-- 6.
type Player = Bool

shownim :: Player -> [Int] -> IO ()
shownim p ns = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] (lines ++ [info])]
               where
                 stars = map (`replicate` '*') ns
                 lines = [(show n) ++ ":" ++ s | (n,s) <- zip [1..] stars]
                 info = case p of
                          True -> "Player1 Turn"
                          _ -> "Player2 Turn"

clearmessage :: IO ()
clearmessage = showmessage "                          "

showmessage :: String -> IO ()
showmessage x = writeat (1,7) x

selectnumber :: String -> IO Int
selectnumber m = do clearmessage
                    showmessage m
                    s <- readLine
                    if (not . null) s && all isDigit s then
                      do return (read s :: Int)
                    else
                      do beep
                         selectnumber m

substar :: Int -> Int -> [Int] -> [Int]
substar n x ns = let (xs,ys) = splitAt n ns
                 in init xs ++ [(last xs) - x] ++ ys

isendnim :: [Int] -> Bool
isendnim = all (<=0)

endnim :: Player -> IO ()
endnim p = do clearmessage
              case p of
                True -> showmessage "Player1 win!"
                _ -> showmessage "Player2 win!"

runnim :: IO ()
runnim = nim True [5,4,3,2,1]

nim :: Player -> [Int] -> IO ()
nim p ns = do cls
              shownim p ns
              n <- selectnumber "select row: "
              x <- selectnumber "remove star: "
              let ns' = substar n x ns
              if isendnim ns' then
                do cls
                   shownim p ns'
                   endnim p
              else
                nim (not p) ns'
