module Tools.Graph
(eulerGraph,
 connected
)
where

import Control.Monad
import Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS

connected :: Graph gr => gr a b -> Bool
connected g 
    | isEmpty g = False
    | otherwise = let (c,_) = matchAny g
                  in (length $ nodes g) == (length $ dfs [node' c] g)

eulerGraph :: DynGraph gr => gr a b -> Maybe [Node]
eulerGraph g = run g [] [] $ circuitPathNode g `mplus` completePathNode g
  where
    run g stack path vertex = do
        v  <- vertex
        let ns = suc g v
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
