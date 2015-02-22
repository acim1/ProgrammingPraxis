module TwoStream where

import Data.List
import Control.Monad
import System.Random

data Stream a = End | Cons a (Stream a)

infixr 5 <:>
(<:>) :: a -> Stream a -> Stream a
(<:>) = Cons

streamFromList :: [a] -> Stream a
streamFromList [] = End
streamFromList (x:xs) = x <:> streamFromList xs

streamToList :: Stream a -> [a]
streamToList End           = []
streamToList (Cons x (xs)) = x : streamToList xs

headStream :: Stream a -> a
headStream (Cons x (xs)) = x

tailStream :: Stream a -> Stream a
tailStream (Cons x (xs)) = xs

stream :: Stream Char
stream = streamFromList $ ['A','D','F','A','G']

randomInt :: RandomGen g => g -> (Int, g)
randomInt = random 

randomPick :: Stream Char -> IO Char
randomPick xs = do
   g <- newStdGen
   return $ randomPick' xs g
   
avg :: Int -> IO ()
avg n = do
    xs <- sequence $ take n $ repeat (randomPick stream)
    forM_ (group . sort $ xs) report
  where
    report ls@(x:xs') = putStrLn $ [x] ++ "=" ++ (show $ fromIntegral (length ls) / (fromIntegral n)) ++ "%"

randomPick' :: RandomGen g => Stream Char -> g -> Char
randomPick' End _ = error "Stream must have at least one element."
randomPick' (Cons x (xs)) g = let (winner,_,_) = foldr f (x,(abs $ goal - n),g'') $ streamToList xs
                              in winner
  where
    f x (y,least,gg) = let (n, gg') = randomInt gg
                           diff     = abs $ goal - n
                           z        = if diff < least then x else y
                       in (z, min diff least, gg')
    (goal, g')  = randomInt g
    (n   , g'') = randomInt g'
    
