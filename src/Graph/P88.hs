module Graph.P88 where

import qualified Data.Map as Map
import Graph.Graph
import Graph.Search

{-
Problem 88: (**) Connected components (alternative solution).

Write a predicate that splits a graph into its connected components.
-}
connectedComponents :: (Ord a) => [a] -> [Edge a] -> [[a]]
connectedComponents vs es = dfs ug $ Map.keys ug
  where
    ug = Map.unionWith (++) (Map.fromList $ map (,[]) vs) (buildUG es)
