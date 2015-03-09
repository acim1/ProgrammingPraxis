module WildcardMatch where

import Data.List

type RegExp = String
type Position = Int

wildcardMatch :: RegExp -> String -> Bool  
wildcardMatch = wc 0

wc :: Position -> RegExp -> String -> Bool
wc p [] ys = True
wc p ('?':xs) (y:ys) = wc (p+1) xs ys
wc p ('?':xs) ys     = False
wc p ('*':xs) []     = wc (p+1) xs []
wc p ('*':xs) ys = or $ zipWith (\p' ys' -> wc p' xs ys') [(p+1)..] (tails ys) 
wc p (x:xs) (y:ys) = if x == y then wc (p+1) xs ys else False
wc p (x:xs) []     = False
