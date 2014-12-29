module MagicSquares where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

data Start  = Top | Bot | Lft | Rht
data Move   = U | D | L | R
type Rule   = [[Move]]
type Order  = Int
type Cell   = (Int,Int)
type Arr    = UArray (Int,Int) Int
type STArr s = STUArray s (Int, Int) Int

findStart :: Order -> Start -> (Int,Int)
findStart n pos = case pos of
                Top -> (1, center)
                Bot -> (n, center)
                Lft -> (center, 1)
                Rht -> (center, n)
  where 
    center = n `quot` 2

magicSquares :: Order -> Start -> Rule -> Arr
magicSquares n start rule = runSTUArray $ do
    a <- newArray ((1,1),(n,n)) 0 :: ST s (STArr s)
    let cell = findStart n start
    ms a n cell rule

-- possibly uneccessary function? do a map of rules over array and be done with it?    
ms :: STArr s -> Order -> Cell -> Rule -> ST s (STArr s)
ms a n c r = go 1
  where 
    go i
      | i > n     = return a
      | otherwise = return a  

