module SevenBridges where

import Data.Graph.Inductive

sevenBridges :: UGr
sevenBridges = undir (g :: UGr)
  where
    g = mkUGraph [1..14] [(1,2),(1,3),(1,5),(2,12),(2,13),(3,4),(3,5),(4,6),(4,8),(4,10),(4,14),(5,6),(6,8),(6,10),(6,14),(7,8),(7,9),(7,11),(8,10),(8,14),(9,10),(9,11),(11,12),(10,14),(12,13),(13,14)]
