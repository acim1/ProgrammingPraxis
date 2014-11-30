module DammsAlgorithm where

import Data.Array
import Data.Char

matrix :: [Int]
matrix = [0,3,1,7,5,9,8,6,4,2,
          7,0,9,2,1,5,4,8,6,3,
          4,2,0,6,8,7,1,3,5,9,
          1,7,5,0,9,8,3,4,2,6,
          6,1,2,3,0,4,5,9,7,8,
          3,6,7,4,2,0,9,5,8,1,
          5,8,6,9,7,2,0,1,3,4,
          8,9,4,5,3,6,2,0,1,7,
          9,4,3,8,6,1,7,2,0,9,
          2,5,8,1,4,3,6,7,9,0
         ] 

arr :: Array (Int,Int) Int
arr = listArray ((0,0),(9,9)) matrix

addCheckDigit :: Int -> Int
addCheckDigit n = go 0 ns
  where
    go cd [] = fromDigits $ ns ++ [cd]
    go cd (m:ms) = go (arr ! (cd,m)) ms 
    ns = toDigits n

verifyCheckDigit :: Int -> Bool
verifyCheckDigit = (== 0) . last . toDigits . addCheckDigit  

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

fromDigits :: [Int] -> Int
fromDigits = read . map (intToDigit) 
