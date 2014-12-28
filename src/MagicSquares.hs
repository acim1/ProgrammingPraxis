module MagicSquares where

import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

data Start  = Top | Bot | Lft | Rht
data Move   = U | D | L | R
type Rule   = [[Move]]
type Order  = Int
type Cell   = (Int,Int)
type Arr    = UArray (Int,Int) Int
type STArr s = ST s (STUArray s (Int, Int) Int)

findStart :: Order -> Start -> (Int,Int)
findStart n pos = case pos of
                Top -> (1, center)
                Bot -> (n, center)
                Lft -> (center, 1)
                Rht -> (center, n)
  where center = n `quot` 2

magicSquares :: Order -> Start -> Rule -> Arr
magicSquares n start rule = runSTUArray $ do
    let a    = newArray ((1,1),(n,n)) 0 :: STArr s
    let cell = findStart n start
    ms a cell rule
    
ms :: STArr s -> Cell -> Rule -> STArr s
ms a c r = undefined 
