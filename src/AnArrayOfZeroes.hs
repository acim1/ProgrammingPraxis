module AnArrrayOfZeroes where

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.STRef

aoz :: [Int] -> Array Int Int
aoz [] = listArray (0,0) []
aoz xs = runSTArray $ do
    right <- newSTRef end 
    arr <- newListArray (0,end) xs
    forM_ [0..end] $ \l -> do
        r <- readSTRef right
        when (l < r) $ do
            x <- readArray arr l
            y <- readArray arr r
            f arr right l r x y 
    return arr
  where
    end = (length xs) - 1
    decr = subtract 1
    f arr right l r 0 0 = do
        when (l < r) $ do
            modifySTRef right (decr)
            r' <- readSTRef right
            y  <- readArray arr r'
            f arr right l r' 0 y
    f arr right l r 0 y = do
        writeArray arr l y
        writeArray arr r 0
        modifySTRef right (decr) 
    f arr right l r _ _ = return ()  
