module Chapter8 (module Chapter8, module Parsing) where

import Parsing

-- 1.
int' :: Parser Int
int' = nat <|> minus
       where minus = do char '-'
                        x <- nat
                        return (- x)
