{-# LANGUAGE RecordWildCards #-}

module Graph.P82 where

import qualified Data.Set as Set
import Graph.Graph
import Graph.Search
import Prelude hiding (cycle)

{-
Problem 82: (*) Cycle from a given node.

Write a predicate cycle to find a closed path (cycle) P starting at a
given node A in the graph G. The predicate should return all cycles
via backtracking.
-}
cycle :: (Ord a) => a -> [Edge a] -> [[a]]
-- Filter out trivial cycles like 1-2-1.
cycle start edges = filter ((> 3) . length) cycles
  where
    cycles = search Search {..}
    ug = buildUG edges
    expand visited =
      filter (\v -> v == start || v `Set.notMember` visited)
        . neighbors ug
    -- Since we start with the start vertex, we need to
    -- make sure that the search doesn't terminate immediately.
    isDone visited u = u == start && (not . Set.null) visited
