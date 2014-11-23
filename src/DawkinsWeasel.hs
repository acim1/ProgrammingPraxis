module DawkinsWeasel where

import System.Random
import Data.List
import Debug.Trace

phrase :: String
phrase = "METHINKS IT IS LIKE A WEASEL"

type Mutation = (Bool,Char)
type Score = Int

type Answer = (String,Iterations)
type Iterations = Int

dw :: IO Answer
dw = do
    g <- getStdGen
    let randomChances = randomRs (0.0 :: Float ,1.0 :: Float) g
    let allowedChars  = ' ' : ['A'..'Z']
    let randomChars   = map (allowedChars !!) $ randomRs (0,26) g
    let mutations     = zipWith (\chance ch -> (chance <= 0.05,ch)) randomChances randomChars
    g' <- newStdGen
    return $ dw' mutations (take (length phrase) $ randomRs ('A','Z') g') 1 
    

dw' :: [Mutation] -> String -> Iterations -> Answer
dw' ms str i = if (fst answer) == (length str)
               then (snd answer,i)
               else dw' ms'' (snd answer) (i+1)
  where
    copies       = replicate 100 str
    (ms'',xs')   = foldr (\x (ms',xs) -> let (ns,ns') = splitAt (length str) ms' in (ns',(let m = mutate x ns in trace ("Mutation: " ++ m) m) : xs)) (ms,[]) copies
    mutate       =  zipWith (\c (b,c') -> if b then c' else c)  
    rank         = sum . (zipWith (\c c' -> if c == c' then 1 else 0) phrase)
    answer       = maximumBy (\x y -> compare (fst x) (fst y)) $ map (\x -> (rank x,x)) xs'
