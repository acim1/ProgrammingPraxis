module FreeTime where

import Data.List
import Data.Maybe

type TimeStr = String
type Min     = Int

data TimeSlot = TimeSlot (Min,Min)

instance Eq TimeSlot where
    (TimeSlot (t1,t2)) == (TimeSlot (t3,t4)) = t1 == t3 && t2 == t4

instance Ord TimeSlot where
    (TimeSlot (_,t1)) `compare` (TimeSlot (_,t2)) = t1 `compare` t2

instance Show TimeSlot where
    show (TimeSlot (t1,t2)) = show (fromMin t1, fromMin t2)    

beg :: TimeSlot -> Min
beg (TimeSlot (t,_)) = t
  
end :: TimeSlot -> Min
end (TimeSlot (_,t)) = t


toMin :: TimeStr -> Min
toMin t = let i = fromJust (findIndex (== ':') t)
              (xs,ys) = splitAt (i+1) t
          in h (init xs) + m ys
  where
    h str = 60 * (read $ leadZeroes str)
    m str = (read . leadZeroes) str
    leadZeroes ('0':xs) = xs
    leadZeroes xs       = xs

fromMin :: Min -> TimeStr
fromMin t = let (x,y) = quotRem t 60 in pad x ++ ":" ++ pad y
  where
    pad n 
      | n >= 10 = show n
      | otherwise = "0" ++ show n


fromList :: [(TimeStr,TimeStr)] -> [TimeSlot]
fromList = map fromPair

fromPair :: (TimeStr,TimeStr) -> TimeSlot
fromPair = (TimeSlot . \(x,y) -> (toMin x, toMin y)) 

merge :: [TimeSlot] -> [TimeSlot] -> [TimeSlot]
merge ts ts'= go (sort $ ts ++ ts')
  where 
        go (t1:t2:ts) = let b1 = beg t1
                            e1 = end t1
                            b2 = beg t2
                            e2 = end t2
                        in 
                            if (e1 >= b2) 
                            then go $ TimeSlot ((min b1 b2),e2) : ts
                            else t1 : go (t2 : ts)  
        go ts@(_) = ts


gaps :: [TimeSlot] -> [TimeSlot]
gaps (t1:t2:ts) = TimeSlot (end t1,beg t2) : gaps (t2 : ts)
gaps _ = []

freetime :: [TimeSlot] -> [TimeSlot] -> [TimeSlot]
freetime ts1 ts2 = gaps (merge ts1 (start : fin : ts2))
  where
    start = fromPair ("00:00","00:00")
    fin   = fromPair ("24:00","24:00") 


freetimeExample = let ts1 = [("8:09","9:45"),("7:10","12:15"),("6:05","11:15")]
                      ts2 = [("8:09","10:45"),("7:10","12:15"),("3:05","4:15")]
                  in freetime (fromList ts1) (fromList ts2)
