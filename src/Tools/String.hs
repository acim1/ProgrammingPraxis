module Tools.String where

import Data.Char

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

fromDigits :: [Int] -> Int
fromDigits = read . map intToDigit
