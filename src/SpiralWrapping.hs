module SpiralWrapping where

data Dir a = Dir Char ((a -> a), (a -> a))

left  = Dir 'L' (id, subtract 1)
right = Dir 'R' (id, (+1))
up    = Dir 'U' (subtract 1, id)
down  = Dir 'D' ((+1), id)

type Pt = (Int,Int)

dirf :: Dir Int -> Pt -> Pt
dirf (Dir _ (f,g)) (x,y) = (f x, g y)

type Bounds = (Int,Int,Int,Int) -- l,d,r,u
type Coord  = (Int,(Int,Int))

spiralWrap :: Bounds -> [Int] -> Pt -> [Coord]
spiralWrap bnds nums start = sw (cycle [left,down,right,up]) bnds nums (start,start)

sw :: [Dir Int] -> Bounds -> [Int] -> (Pt,Pt) -> [Coord]
sw _ _ [] _ = []
sw dirs@(d:next:ds) bnds (v:vs) (pt,lstpt) 
    | outofbounds pt bnds = sw (next:ds) (newbnds d bnds) (v:vs) (dirf next lstpt, lstpt) 
    | otherwise = (v,pt) : sw dirs bnds vs (dirf d pt,pt) 

outofbounds (x,y) (l,d,r,u) = x < l || x > r || x < u || x > d

newbnds :: Dir Int -> Bounds -> Bounds
newbnds (Dir 'L' _) (l,d,r,u) = (l,d,r,u+1)
newbnds (Dir 'D' _) (l,d,r,u) = (l+1,d,r,u)
newbnds (Dir 'R' _) (l,d,r,u) = (l,d-1,r,u)
newbnds (Dir 'U' _) (l,d,r,u) = (l,d,r-1,u)


-- try this if you dare...you crazy SOB... 
