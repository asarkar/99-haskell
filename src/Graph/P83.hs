{-# LANGUAGE RecordWildCards #-}

module Graph.P83 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Graph.Graph
import Graph.Search

{-
Problem 83: (**) Construct all spanning trees.
-}
spanningTrees :: (Ord a) => [Edge a] -> [[a]]
spanningTrees edges = concatMap go $ vertices ug
  where
    go start = search Search {..}
    ug = buildUG edges
    expand visited = filter (`Set.notMember` visited) . neighbors ug
    isDone visited _ = 1 + Set.size visited == Map.size ug
