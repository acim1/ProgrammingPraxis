module TwoStream where

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
stream = streamFromList $ replicate 1 'A' ++ replicate 2 'D' ++ 
         replicate 5 'F' ++ replicate 3 'A' ++ replicate 9 'G'

randomInt :: RandomGen g => g -> (Int, g)
randomInt = random 

randomPick :: Stream Char -> IO Char
randomPick xs = do
   g <- newStdGen
   return $ randomPick' xs g 

randomPick' :: RandomGen g => Stream Char -> g -> Char
randomPick' End _ = error "Stream must have at least one element."
randomPick' (Cons x (xs)) g = let (winner,_,_) = foldr f (x,diff,g'') $ streamToList xs
                              in winner
  where
    f x (y,diff,gg) = let (n, gg') = randomInt gg
                          diff' = abs $ goal - n
                          z     = if diff' < diff then y else x
                     in (z, min diff diff', gg')
    (goal, g')  = randomInt g
    (diff, g'') = randomInt g'
    
