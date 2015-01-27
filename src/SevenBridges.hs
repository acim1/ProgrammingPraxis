module SevenBridges where

import Control.Monad
import Data.Graph.Inductive

sevenBridges :: UGr
sevenBridges = undir (g :: UGr)
  where
    g = mkUGraph [1..3] [(1,2),(1,3),(2,3)] 

eulerGraph :: DynGraph gr => gr a b -> Maybe [Node]
eulerGraph g = run g [] [] $ circuitPathNode g `mplus` completePathNode g
  where
    run g stack path vertex = do
        v  <- vertex
        let ns = neighbors g v
        case (stack,ns) of
            ([],[])   -> return (v:path)
            (_,n:ns)  -> do
                let g' = delEdges [(v,n),(n,v)] g
                run g' (v:stack) path (return n)
            (x:xs,_) -> do
                run g xs (v:path) (return x)

circuitPathNode :: Graph gr => gr a b -> Maybe Node    
circuitPathNode g = if p then Just (node' . fst . matchAny $ g) else Nothing 
  where
    p = ufold (\c u -> u && (even . deg') c) True g

completePathNode :: Graph gr => gr a b -> Maybe Node
completePathNode g = toMaybe odds   
  where
    odds   = ufold f [] g
    f c ns = let n  = node' c
                 d  = deg'  c
             in if odd d then n : ns else ns
    toMaybe (x:y:[]) = Just x
    toMaybe _        = Nothing             
