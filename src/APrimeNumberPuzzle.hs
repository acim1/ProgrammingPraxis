module APrimeNumberPuzzle where

import Tools.Math
import Data.Char
import Debug.Trace

pnp :: [Int]
pnp = map pnp' [1..9]

pnp' :: Int -> Int
pnp' n
    | let m = (show n) in length m == (digitToInt . head) m = n   
    | otherwise = pnp' n'
  where
    n' = read $ show n ++ (tail . show) nextPrime   
    nextPrime =
        let i  = n `mod` 10
            twos (_:[]) = []
            twos (x:y:xys) = [x,y] : twos (y:xys)
            p xs@(c:_) = (digitToInt c == i && length xs == 2)
                         && (notElem xs (twos $ show n))
        in head . filter (p . show) $ primes    
