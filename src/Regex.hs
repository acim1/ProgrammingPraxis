module Regex where

import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe

type RegexString = String
type Regex = [Exp]

data Exp = Beg | End | Esc Char | Lit Char | Any | CharClass ChrClass| ZeroOrMore deriving Show 

data ChrClass = ChrClass Regex | Rng (Char,Char) | Neg ChrClass deriving Show


parse :: String -> Maybe Regex
parse [] = Nothing
parse ('*':xs) = Nothing
parse ('^':xs) = Just (Beg : ) <*> prs xs
parse xs       = prs xs

prs :: String -> Maybe Regex
prs []               = Just []
prs ('\\' : c : xs)  = Just (Esc c :) <*> prs xs
prs ('\\' : [])      = Nothing 
prs ('^'  : _ )      = Nothing
prs ('$'  : [])      = Just [End]
prs ('$'  : _)       = Nothing
prs ('.'  : xs)      = Just (Any :) <*> prs xs
prs ('*'  : xs)      = Just (ZeroOrMore :) <*> prs xs 
prs ('['  : xs)      = case findIndex (== ']') xs of
                         (Just i) -> 
                           let (chClass, xs') = splitAt i xs
                           in ((:) <$> (CharClass <$> prsChCls chClass)) <*> prs (tail xs')
                         _        -> Nothing
prs (']'  : xs)      = Nothing
prs (c    : xs)      = Just (Lit c :) <*> prs xs
    

prsChCls :: String -> Maybe ChrClass
prsChCls ('^' : '^' : xs)         = Nothing    
prsChCls ('^' : xs)               = Just Neg <*> prsChCls xs
prsChCls xs@('\\' : '-' : x : []) = ChrClass <$> expr xs
prsChCls (i : '-' : j : [])       = if i < j then rng (i,j) else Nothing
prsChCls xs = ChrClass <$> expr xs

expr :: String -> Maybe Regex
expr xs = parse xs >>= validate
  where  
    validate []           = Just []
    validate (Esc c : xs) = Just (Esc c :) <*> validate xs
    validate (Lit c : xs) = Just (Lit c :) <*> validate xs
    validate _            = Nothing

     
rng :: (Char,Char) -> Maybe ChrClass 
rng (i,j)
 | (isDigit i) && (isDigit j) = Just $ Rng (i,j)
 | (isUpper i) && (isUpper j) = Just $ Rng (i,j)
 | (isLower i) && (isLower j) = Just $ Rng (i,j)
 | otherwise                  = Nothing


match :: RegexString -> String -> Bool
match regexStr str = case parse regexStr of
                (Just regex) -> regexMatch regex str
                _            -> error "Invalid regular expression."

regexMatch :: Regex -> String -> Bool
regexMatch (Beg:exps) xs = rMatch exps xs
regexMatch exps xs = or $ map (rMatch exps) (tails xs)

rMatch :: Regex -> String -> Bool
rMatch (End:[]) [] = True
rMatch (End:[]) _  = False
rMatch [] _        = True
rMatch regex@(exp : ZeroOrMore : exps) str = 
    let newStr = eat exp str 
    in if eaten str newStr then rMatch regex newStr else rMatch exps str
rMatch (exp : exps) str = 
    let newStr = eat exp str 
    in if eaten str newStr then rMatch exps newStr else False                                                  
    
eaten :: String -> String -> Bool    
eaten oldStr newStr = (length oldStr) > (length newStr)

eat :: Exp -> String -> String
eat = undefined                                               
                                                             
                
                                    

