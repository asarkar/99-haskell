{-# LANGUAGE RecordWildCards #-}

module Graph.P81 where

import qualified Data.Set as Set
import Graph.Graph
import Graph.Search

-- Problem 81: (**) Paths between two given nodes.
paths :: (Ord a) => a -> a -> [Edge a] -> [[a]]
paths start end edges = search Search {..}
  where
    ug = buildUG edges
    expand visited = filter (`Set.notMember` visited) . neighbors ug
    isDone = const (end ==)
