module Chapter11 (module Chapter11, choices) where

import CountDown




choices' :: [a] -> [[a]]
choices' xs = concat [perms ys | ys <- subs xs]
