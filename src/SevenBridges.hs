module SevenBridges where

import Control.Monad
import Data.Graph.Inductive
import Tools.Graph

sevenBridges :: Gr Char ()
sevenBridges = g
  where
    g = mkGraph (zip [1..4] ['a'..'d']) [(1,2,()),(1,2,()),(1,3,()),(2,1,()),(2,1,()),(2,3,()),(2,4,()),(2,4,()),(3,1,()),(3,2,()),(3,4,()),(4,2,()),(4,2,()),(4,3,())] 

runSevenBridges = eulerGraph sevenBridges
