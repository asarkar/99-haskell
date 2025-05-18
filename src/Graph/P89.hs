module Graph.P89 where

import qualified Data.List as L
import Graph.Graph
import Graph.P86

{-
Problem 89: (**) Bipartite graphs.

Write a predicate that finds out whether a given graph is bipartite.

ANSWER:
A bipartite graph is always 2-colorable, and vice-versa.
-}
bipartite :: (Ord a) => [a] -> [Edge a] -> Bool
bipartite vs = (== 2) . length . L.nub . map snd . kColor vs
