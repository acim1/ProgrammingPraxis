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
    center = (n `quot` 2) + 1

magicSquares :: Order -> Start -> Rule -> Arr
magicSquares n start rule = runSTUArray $ do
    a <- newArray ((1,1),(n,n)) 0 :: ST s (STArr s)
    let i = findStart n start
    writeArray a i 1
    foldM_ (applyRule a n rule) i [2..(n*n)]
    return a

applyRule :: STArr s -> Order -> Rule -> Cell -> Int -> ST s Cell
applyRule a n r c x = go r
  where 
    go (m:[]) = write (seek n m c)
    go (m:ms) = do 
        let i = seek n m c
        e <- readArray a i
        if e == 0 then write i else go ms
    write i = writeArray a i x >> return i

seek :: Order -> [Move] -> Cell -> Cell
seek n ms c = foldl go c ms
  where
    go (i,j) m = case m of
        U -> wrap (i-1,j)
        D -> wrap (i+1,j)
        L -> wrap (i,j-1)
        R -> wrap (i,j+1)
    wrap (i,j) = (f i, f j)
    f k 
     | k == 0     = n
     | k == (n+1) = 1
     | otherwise  = k 

