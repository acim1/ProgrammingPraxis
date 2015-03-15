module Regex where

import Data.List
import Control.Applicative

type Regex = [Exp]

data Exp = Beg | End | Esc Char | Lit Char | Any | CharClass | ZeroOrMore 


parse :: String -> Maybe Regex
parse [] = Nothing
parse ('*':xs) = Nothing
parse ('^':xs) = Just (Beg : ) <*> prs xs
parse xs       = prs xs
  where
    prs []               = Just []
    prs ('\\' : c : xs)  = Just (Esc c :) <*> prs xs
    prs ('\\' : [])      = Nothing 
    prs ('$'  : [])      = Just (End : [])
    prs ('$'  : _)       = Nothing
    prs ('.'  : xs)      = Just (Any :) <*> prs xs
    prs ('*'  : xs)      = Just (ZeroOrMore :) <*> prs xs 
    prs ('['  : xs)      = case findIndex ']' of
                               (Just i) -> 
                                   let (chClass, xs') = splitAt i xs
                                   in fmap (:) (parseCharClass chClass) <*> prs (tail xs')
                               _        -> Nothing
    prs (']'  : xs)      = Nothing
    
    
parseCharClass = undefined   
