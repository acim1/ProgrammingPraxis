module DianaCryptosystem where

import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
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

trigraph :: M.Map Char [(Char, Char)]
trigraph = M.fromList [(ltr, zip abc (shift revabc i)) | (ltr,i) <- zip abc [0..25]]
 where
    revabc = reverse abc

readtg :: Char -> Char -> Maybe Char
readtg r c = lookup c =<< M.lookup r trigraph

decipher :: [String] -> Maybe [String]
decipher xs
    | length xs < 2 = Nothing
    | otherwise = let prefix = take 2 xs 
                      cipher = drop 2 xs
                      key    = drop 2 <$> find (isPrefixOf prefix) (tails onetimepad)
                  in (prefix ++) <$> go cipher key  
  where
    go _  Nothing    = Nothing
    go xs (Just ys)  = zipWithM f xs ys
    f x y            = zipWithM readtg x y     

encipher :: [String] -> Maybe [String]
encipher = decipher 
