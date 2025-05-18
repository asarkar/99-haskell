{-# LANGUAGE LambdaCase #-}

module Graph.P84 where

import qualified Control.Monad.State as S
import qualified Data.HashPSQ as Q
import Data.Hashable (Hashable)
import qualified Data.Set as Set

{-
Problem 84: (**) Construct the minimal spanning trees.

ANSWER: We use Prim's eager MST algorithm.

https://www.youtube.com/watch?v=xq3ABa-px_g

- Maintain a min Indexed Priority Queue (IPQ) of size V
  that sorts vertex-edge pairs based on the min edge cost
  of e. By default, all vertices v have a best value of ∞
  in the IPQ.

- Start the algorithm on any node 's'. Mark s as visited
  and relax all edges of s.
  Relaxing refers to updating the entry for node v in the
  IPQ from (v, old_edge) to (v, new_edge) if the new_edge
  from u -> v has a lower cost than old_edge.

- While the IPQ is not empty and a MST has not been formed,
  deque the next best (v, e) pair from the IPQ. Mark node v
  as visited and add edge e to the MST.

- Next, relax all edges of v while making sure not to relax
  any edge pointing to a node which has already been visited.

This algorithm runs in O(E log V) time since there can only
be V (node, edge) pairs in the IPQ, making the update and
poll operations O(log V).
-}
prim :: (Ord a, Hashable a) => [(a, a, Int)] -> [(a, a, Int)]
prim edges = S.evalState go initialState
  where
    (u0, _, _) = head edges
    -- Start with all edges incident to u0 on the heap.
    initialState = (Set.singleton u0, relax Q.empty (outE u0))
    -- Sorts the given edge so that vertex u appears first.
    sortE u (x, y, cost) = if x == u then (x, y, cost) else (y, x, cost)
    -- Determines if the given edge is incident to u.
    isIncidentTo u (x, y, _) = x == u || y == u
    -- Finds all edges incident to u, and sorts them so that u appears first.
    outE u = (map (sortE u) . filter (isIncidentTo u)) edges
    -- Relaxes the given edges.
    relax = foldl update
    -- If the edge (from-to) is not present on the heap, inserts it,
    -- otherwise if the edge on the heap has a greater cost,
    -- replaces it with the given edge.
    update q (from, to, cost) =
      snd $
        Q.alter
          ( \case
              Nothing -> ((), Just (cost, from))
              Just (p, v) ->
                if p <= cost
                  then ((), Just (p, v))
                  else ((), Just (cost, from))
          )
          to
          q
    go = do
      (visited, q) <- S.get
      -- Edges are put on the queue as v: (u, priority),
      -- where the edge is incident to the vertex 'v'
      -- from the vertex 'u'. The edge cost is the priority.
      case Q.minView q of
        Nothing -> return []
        -- At each iteration, pick an edge (v, u) with the minimum cost,
        -- and relax all edges incident to v, except for those connected
        -- to vertices already visited.
        Just (to, cost, from, rest) -> do
          let out = filter (\(_, v, _) -> v `Set.notMember` visited) $ outE to
          let q' = relax rest out
          let visited' = Set.insert to visited
          S.put (visited', q')
          xs <- go
          return $ (from, to, cost) : xs
