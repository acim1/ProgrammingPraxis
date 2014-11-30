module Tools.Math where

import Data.List
import Data.Maybe
import Data.Functor
import Data.Array

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

lpf :: Int -> Int
lpf x
    | isPrime x = x
    | otherwise =
        let pf = fromJust $ find (\p -> x `rem` p == 0) primes
            n  = x `quot` pf
        in lpf n

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x
    | even x = False
    | otherwise =
        case divisor of
            Nothing  -> True
            _        -> False
  where
    divisor = find (\d -> x `rem` d == 0) $ takeWhile (<= n) [3,5..]
    n = (ceiling . sqrt . fromIntegral) x

primes :: [Int]
primes = 2 : sieve 1 [3,5..]

sieve :: Int -> [Int] -> [Int]
sieve n (x:xs) =
    -- if we haven't found a divisor before reaching square root of x, we won't find one after
    let ps = takeWhile (\p-> p*p <= x) $ take n primes
    in
        if (any (\p -> x `rem` p == 0) ps)
        then sieve n xs
        else x : sieve (n+1) xs

primeFactorization :: Int -> [Int]
primeFactorization x
    | isPrime x = [x]
    | otherwise = p : primeFactorization q
  where
    (p,q) = head $ [(p,q) | p <- primes, let (q,m) = divMod x p, m == 0]

smallestMult :: Int -> Int
smallestMult n = product $ foldr merge [] $ map primeFactorization [2..n]
  where
    merge ns' ns = (ns \\ ns') ++ ns'

lrgProd :: [Int] -> Int -> Int
lrgProd xs n = lrgProd' xs n 0

lrgProd' :: [Int] -> Int -> Int -> Int
lrgProd' xs n lrg
    | length (take n xs) < n = lrg
    | otherwise =
        let prod = product (take n xs)
        in lrgProd' (drop 1 xs) n (max lrg prod)

readDigits :: FilePath -> IO [Int]
readDigits fp = do
    number <- readFile fp
    let digits = map (read . return) $ concat (lines number)
    return digits

pyTriplet :: (Int,Int,Int)
pyTriplet = head $ [ pt | m <- [1..32], n <- [(m+1)..32], let pt@(a,b,c) = mkPyTriple m n, a+b+c == 1000 ]
  -- going up to 32 because 32^2 > 1000 (c = n^2 + m^2...). Not optimal, but limitation enough to prevent infinity.

mkPyTriple :: Int -> Int -> (Int,Int,Int)
mkPyTriple m n
    | m >= n = error "m must be less than n"
    | otherwise = (a,b,c) -- a and b may be interchangeable
  where
    a = n^2 - m^2
    b = 2 * n * m
    c = n^2 + m^2


hiDivTriangleNum :: Int -> Int
hiDivTriangleNum n = head . (filter divs) $ tail triangleNums -- tail because can't prime factor "1"
  where
    divs x = (>= n) $ (length . nub . subsequences . primeFactorization) x

triangleNums :: [Int]
triangleNums = 1 : zipWith (+) [2..] triangleNums

subsequences' :: [a] -> [[a]]
subsequences' xs = f [[]] $ map return xs
  where
    f xxs yys = foldr (\ys r -> r ++ [xs ++ ys | xs <- r]) xxs yys
