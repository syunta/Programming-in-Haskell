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
