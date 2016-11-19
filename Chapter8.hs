module Chapter8 (module Chapter8, module Parsing) where

import Parsing

-- 1.
int' :: Parser Int
int' = nat <|> minus
       where minus = do char '-'
                        x <- nat
                        return (- x)

-- 2.
comment :: Parser ()
comment = do string "--"
             many (sat (/='\n'))
             char '\n'
             return ()
-- 3.
{-

2 + 3 + 4
       expr
     /   |   \
expr     +   expr
 |            |
term         expr
 |         /  |   \
factor  expr  +   expr
 |       |          |
nat     term      term
 |       |          |
 2      factor    factor
         |          |
        nat        nat
         |          |
         3          4

2 + 3 + 4
            expr
         /   |   \
     expr    +   expr
      |            |
     expr        term
   /  |   \        |
expr  +   expr   factor
 |          |      |
term      term    nat
 |          |      |
factor    factor   4
 |          |
nat        nat
 |          |
 2          3

-}

-- 4.
{-

2 + 3
       expr
     /   |   \
term     +   expr
 |             |
factor       term
 |             |
nat          factor
 |             |
 2            nat
               |
               3

2 * 3 * 4
       expr
         |
       term
     /   |   \
factor   *    term
 |          /   |   \
nat     factor  *   term
 |        |          |
 2       nat       factor
          |          |
          3         nat
                     |
                     4

(2 + 3) * 4
              expr
               |
              term
           /   |   \
       factor  *   term
     /   |   \       |
   (    expr   )   factor
      /  |   \       |
  term   +   expr   nat
    |          |     |
  factor     term    4
    |          |
   nat       factor
    |          |
    2         nat
               |
               3
-}

-- 5.
{-

最後の簡略化がない場合、3 のような式では次のように解析されうる

term -> factor -> nat 3 の解析後、続く文字に + がないので単項であると分かり、

もう一度 term -> factor -> nat 3 の解析を行う

簡略化をすると解析は一回で済む。

-}

-- 6. 7.

{-

expr ::= term ('+' expr | '-' expr | ε)
term ::= expt ('*' term | '/' term | ε)
expt ::= factor ('^' expr | ε)
factor ::= '(' expr ')' | nat
nat ::= '0' | '1' | '2' | ...

-}

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           <|>
           do symbol "-" -- added for 6
              e <- expr
              return (t - e)
           <|>
           return t

term :: Parser Int
term = do e <- expt -- modified for 7
          do symbol "*"
             t <- term
             return (e * t)
           <|>
           do symbol "/" -- added for 6
              t <- term
              return (e `div` t) -- 型の変更が面倒なので div を使用
           <|>
           return e

expt :: Parser Int -- added for 7
expt = do f <- factor
          do symbol "^"
             e <- expr
             return (f ^ e)
           <|>
           return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> natural

eval :: String -> Int
eval xs = case (parse expr xs) of
             [(n,[])]  -> n
             [(_,out)] -> error ("Unused input " ++ out)
             []        -> error "Invalid input"
-- 8.
-- a.
{-

expr ::= expr '-' nat | nat
nat ::= '0' | '1' | '2' | ...

-}

-- b.
expr' :: Parser Int
expr' = do e <- expr'
           symbol "-"
           n <- nat
           return (e - n)
         <|>
         do n <- nat
            return n

-- c.
-- 上記のように素直に実装すると無限ループに陥る

-- d.
expr'' :: Parser Int
expr'' = do n <- nat
            ns <- many (do symbol "-"
                           n' <- nat
                           return n')
            return (foldl (-) n ns)
