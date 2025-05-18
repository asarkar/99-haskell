module Graph.Graph where

import Control.Monad ((<=<))
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map

-------------------------------------------------------------------------
--                                                                      -
--      Graph utilities
--                                                                      -
-------------------------------------------------------------------------
type Graph a = Map a [a]

type Edge a = (a, a)

-- Builds a directed graph.
buildG :: (Ord a) => [Edge a] -> Graph a
buildG = foldr merge Map.empty
  where
    -- insertWith called with key, f new_value old_value,
    -- and new_value is a singleton.
    merge (u, v) = Map.insertWith ((:) . head) u [v]

-- Reverses the edges.
reverseE :: [Edge a] -> [Edge a]
reverseE = map (\(u, v) -> (v, u))

-- Builds an undirected graph.
-- Since the graph may contain cycles, make sure
-- to not include the same vertex more than once.
buildUG :: (Ord a) => [Edge a] -> Graph a
buildUG edges = Map.unionWith ((L.nub .) . (++)) g g'
  where
    g = buildG edges
    g' = (buildG . reverseE) edges

vertices :: (Eq a) => Graph a -> [a]
vertices = L.nub . (uncurry (:) <=< Map.toList)

neighbors :: (Ord a) => Graph a -> a -> [a]
neighbors = flip (Map.findWithDefault [])
