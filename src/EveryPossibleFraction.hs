module EveryPossibleFraction where

import Data.Ratio

runEPF :: [String]
runEPF = map show' epf
  where
    show' x = show (numerator x) ++ "/" ++ show (denominator x)

epf :: [Rational]
epf = epf' one
  where
    epf' x = let x' = one / (n - y + one)
                 n  = floor x % 1
                 y  = x - n
             in x : epf' x'
    one = 1 % 1

