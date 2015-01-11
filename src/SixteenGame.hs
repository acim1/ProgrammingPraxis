
module SixteenGame where

import Control.Monad
import Data.List
import System.Random


playGame :: Int -> IO ()
playGame n = do
    games <- forM (replicate n game) display
    let xs = filter snd games
    let percentage = 100 * ((fromIntegral $ length xs) / (fromIntegral n))
    putStrLn $ "Winning Percentage: " ++ show percentage ++ "%"
  where
    display f = do
        g <- newStdGen
        let x = f g
        -- putStrLn $ show x
        return x

winningNums, losingNums :: [Int]
winningNums = [1,2]
losingNums  = [3]

game :: RandomGen g => g -> ([Int],Bool)
game g = (nums,win winningNums losingNums nums)
  where 
    nums = unfoldr next ([1..16],g)
    next ([],g) = Nothing
    next (xs,g) = let (i,g')  = randomR (0, (length xs)-1) g
                      (x,xs') = remove i xs
                     in Just (x,(xs',g'))

win :: [Int] -> [Int] -> [Int] -> Bool
win [] _ _ = True
win winners losers (n:ns) 
    | n `elem` losers = False
    | otherwise = win (delete n winners) losers ns    
                     

remove :: Int -> [a] -> (a,[a])
remove i xs 
    | i+1 > length xs = error "Inaccessible index."
    | otherwise = remove' 0 (head xs,xs)
  where
    remove' j (_,(x:xs))  
        | i == j = (x,xs)
        | otherwise = let (y,ys) = remove' (j+1) (x,xs) in (y,x:ys)                     
                        
