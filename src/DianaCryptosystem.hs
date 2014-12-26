module DianaCryptosystem where

import Control.Applicative
import Data.List
import Data.Map.Strict (Map,fromList,(!))
import Data.Maybe (fromJust)

abc :: String
abc = ['A'..'Z']

shift ::  [a] -> Int -> [a]
shift xs n = let (xs',xs'') = splitAt n xs
             in xs'' ++ xs'


onetimepad :: [String]
onetimepad = ["WHTVI","AUCFU","RETFK","OMSAL",
              "MYMNE","ZIEGP","UKVTF","WZHOK",
              "GORWY","WETFR","COYET","OOWHY",
              "ZPDDA","CMMXT","VYTJI","RRQGU"]             

trigraph :: Map Char [(Char, Char)]
trigraph = fromList [(ltr, zip abc (shift revabc i)) | (ltr,i) <- zip abc [0..25]]
 where
    revabc = reverse abc

readtg :: Char -> Char -> Maybe Char
readtg r c = lookup c (trigraph ! r)

decipher :: [String] -> Maybe [String]
decipher xs
    | length xs < 2 = Nothing
    | otherwise = let cipher = drop 2 xs
                      key    = drop 2 <$> find (isPrefixOf xs) (tails onetimepad)
                  in (take 2 xs ++) <$> go cipher key  
  where
    go _  Nothing    = Nothing
    go xs (Just ys)  = undefined     


 
