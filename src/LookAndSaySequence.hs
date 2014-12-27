module LookAndSay where

import Data.List

lookAndSay :: [[Int]]
lookAndSay = iterate f [1,1]
  where
    f          = (concatMap g . group)
    g xs@(x:_) = [length xs, x] 
