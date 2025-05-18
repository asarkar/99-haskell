module Graph.P87 where

import qualified Data.Map as Map
import Graph.Graph
import Graph.Search

{-
Problem 87: (**) Depth-first order graph traversal (alternative solution).

Write a predicate that generates a depth-first order graph traversal sequence.
The starting point should be specified, and the output should be a list of nodes
that are reachable from this starting point (in depth-first order).
-}
depthFirst :: (Ord a) => [a] -> [Edge a] -> a -> [a]
depthFirst vs es st = head forest
  where
    ug = Map.unionWith (++) (Map.fromList $ map (,[]) vs) (buildUG es)
    forest = dfs ug [st]
