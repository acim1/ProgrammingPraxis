module WordChaining where

import qualified Data.Map as M
import Data.List

words' :: [String]
words' = "eve" : "eat" : "ripe" : "tear" : []

charMap :: (String -> Char) -> M.Map Char [String]
charMap g = foldr f M.empty $ zip (map g words') words'
  where f (k,v) m = let vs = M.findWithDefault [] k m
                    in M.insert k (v:vs) m

firstMap = charMap head
lastMap  = charMap last

chain :: [String] -> Bool
chain (x:[]) = False
chain xs = or $ map f ps
  where
    f (x:xs) = undefined 
    ps       = permutations xs


findAndRemove f [] = Nothing
findAndRemove f (x:xs)
            | f x = Just (x,xs)
            | otherwise = fmap (\(y,ys) -> (y, x:ys)) $ findAndRemove f xs

