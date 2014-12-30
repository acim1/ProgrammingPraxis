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
    foldM_ (applyRule a rule) cell [1..n]
    return a

applyRule :: STArr s -> Rule -> Cell -> Int -> ST s Cell
applyRule a r c i = go r
  where 
    go (r:[]) = apply r
    go (r:rs) = if check r then apply r else go rs
    check r   = undefined
    apply r   = undefined
