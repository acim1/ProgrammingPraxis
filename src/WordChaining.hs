module WordChaining where

import Control.Applicative ((<$>))
import Data.Graph.Inductive
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Tools.Graph

words' :: [String]
words' = "eve" : "eat" : "ripe" : "tear" : []

charMap :: (String -> Char) -> [String] -> M.Map Char [String]
charMap g wrds = foldr f M.empty $ zip (map g wrds) wrds
  where f (k,v) m = let vs = M.findWithDefault [] k m
                    in M.insert k (v:vs) m

firstMap = charMap head
lastMap  = charMap last


-- needs alterations to work with duplicate words
mkWordGraph :: [String] -> Gr String (String,String)
mkWordGraph []   = empty
mkWordGraph wrds = mkGraph (zip [1..] wrds) $ map (\xy@(x,y) -> (lblMap M.! x, lblMap M.! y, xy)) (nub $ concatMap mkEdges wrds) 
    where
      mkEdges w = let a = head w
                      z = last w
                      preds = map (\w' -> (w',w)) $ filter (/= w) $ lm M.! a
                      succs = map (\w' -> (w,w')) $ filter (/= w) $ fm M.! z
                  in preds ++ succs
      lblMap = M.fromList $ zip wrds [1..]
      revTuples = map (\(x,y) -> (y,x))
      fm = firstMap wrds
      lm = lastMap  wrds

chain :: [String] -> Maybe [String]
chain wrds = let g = mkWordGraph wrds
             in (listToMaybe . catMaybes) $ map (dfs' g) (nodes g) 
  where
    dfs' g n = case match n g of
                 (Just c,g') ->
                    let label = lab' c
                        ns    = suc' c
                        none  = isEmpty g'
                    in 
                        case (null ns, none) of
                            (True, True)  -> Just [label]
                            (True, False) -> Nothing
                            _             -> 
                               fmap (label :) $ (listToMaybe . catMaybes) $ map (dfs' g') ns 
                 _           -> Nothing


findAndRemove f [] = Nothing
findAndRemove f (x:xs)
            | f x = Just (x,xs)
            | otherwise = fmap (\(y,ys) -> (y, x:ys)) $ findAndRemove f xs

