module DianaCryptosystem where

import Data.List
import Data.Map.Strict (Map,fromList,(!))
import Data.Maybe (fromJust)

abc :: String
abc = ['A'..'Z']

shift ::  [a] -> Int -> [a]
shift xs n = let (xs',xs'') = splitAt n xs
             in xs'' ++ xs'

trigraph :: Map Char [(Char, Char)]
trigraph = fromList [(ltr, zip abc (shift revabc i)) | (ltr,i) <- zip abc [0..25]]
 where
    revabc = reverse abc

readtg :: Char -> Char -> Char
readtg r c = fromJust $ lookup c (trigraph ! r)

 
