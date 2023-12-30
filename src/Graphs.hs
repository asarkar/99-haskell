{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Graphs
  ( paths,
    cycle,
    spanningTrees,
    prim,
    iso,
    kColor,
    bipartite,
    depthFirst,
    connectedComponents,
  )
where

import Control.Monad as M
import qualified Control.Monad.State as S
import qualified Data.Bifunctor as Bf
import qualified Data.HashPSQ as Q
import Data.Hashable (Hashable)
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Mb
import Data.Ord (Down (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (cycle)

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

-------------------------------------------------------------------------
--                                                                      -
--      Algorithms
--                                                                      -
-------------------------------------------------------------------------

data Search a = Search
  { start :: a,
    -- shouldInclude visited vertex
    expand :: Set a -> a -> [a],
    isDone :: Set a -> a -> Bool
  }

-- Backtracking DFS that may visit the same
-- vertex more than once from another path.
search :: (Ord a) => Search a -> [[a]]
search (Search start expand isDone) = go Set.empty start
  where
    go visited u
      | isDone visited u = [[u]]
      | otherwise = do
          let visited' = Set.insert u visited
          v <- expand visited' u
          xs <- go visited' v
          M.guard (xs /= [])
          return $ u : xs

{-
Problem 80: (***) Conversions.

Write predicates to convert between the different graph representations.
With these predicates, all representations are equivalent; i.e. for the
following problems you can always pick freely the most convenient form.
The reason this problem is rated (***) is not because it's particularly
difficult, but because it's a lot of work to deal with all the special cases.

ANSWER: TODO.
-}

-- Problem 81: (**) Paths between two given nodes.
paths :: (Ord a) => a -> a -> [Edge a] -> [[a]]
paths start end edges = search Search {..}
  where
    ug = buildUG edges
    expand visited = filter (`Set.notMember` visited) . neighbors ug
    isDone = const (end ==)

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

{-
Problem 84: (**) Construct the minimal spanning trees.

ANSWER: Prim's eager MST algorithm.
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
    -- Sorts the given edge so that u appears first.
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

{-
Problem 85: (**) Graph isomorphism.

Two graphs G1(N1,E1) and G2(N2,E2) are isomorphic if there is a bijection
f: N1 -> N2 such that for any nodes X,Y of N1, X and Y are adjacent
if and only if f(X) and f(Y) are adjacent.

Write a predicate that determines whether two graphs are isomorphic.

ANSWER:
https://davidbieber.com/post/2019-05-10-weisfeiler-lehman-isomorphism-test/

https://en.wikipedia.org/wiki/Weisfeiler_Leman_graph_isomorphism_test

- At each iteration, the algorithm assigns to each node a tuple
  containing the node's old compressed label and a list of the
  node's neighbors' compressed labels. This is the node's new
  "uncompressed" label.
- The algorithm then groups uncompressed labels and assign a unique
  id to each group that is the "compressed" label for that group.
- If the number of groups is the same as the number of groups in the
  previous iteration, the algorithm does the following:
  - The compressed labels are reduced to a "canonical" form which is
    a sorted list of tuples of the form (label, count).
  - If two graphs have the same canonical form, they may be isomorphic.
    If not, they are certainly not isomorphic.
- If the number of groups is not the same, the algorithm assigns compressed
  labels to each node and continues to the next iteration.
  Any two nodes with the same uncompressed label will get the same
  compressed label.

- The algorithm starts by assigning each node the same compressed label, 0.
- One possible convention for creating compressed labels is to use increasing
  integers starting from 1.

The core idea of the Weisfeiler-Lehman isomorphism test is to find for each
node in each graph a signature based on the neighborhood around the node.
These signatures can then be used to find the correspondance between nodes
in the two graphs, which can be used to check for isomorphism.

In the algorithm descibed above, the "compressed labels" serve as the signatures.
-}
iso :: (Ord a, Ord b) => [a] -> [Edge a] -> [b] -> [Edge b] -> Bool
iso v1 e1 v2 e2 = m == n && go 0 0 (map (,0) v1) (map (,0) v2) 1
  where
    ug1 = Map.unionWith (++) (Map.fromList $ map (,[]) v1) (buildUG e1)
    ug2 = Map.unionWith (++) (Map.fromList $ map (,[]) v2) (buildUG e2)
    m = length v1
    n = length v2

    -- Find old label.
    label :: forall a. (Eq a) => [(a, Int)] -> a -> Int
    label cl = Mb.fromJust . flip L.lookup cl
    -- Given the neighbors and their compressed labels,
    -- compute new uncompressed label for this vertex.
    uncompress cl = L.sort . map (label cl)
    -- Group uncompressed labels, and assign a compressed label to each group.
    group xxs labelId =
      zipWith
        (\xs k -> (head xs, (length xs, k)))
        (L.group $ L.sort xxs)
        [labelId + 1 ..]
    -- Assign compressed label to each group.
    compress xs xxs = map (snd . Mb.fromJust . flip L.lookup xxs) xs
    -- Reduce the graph into canonical form.
    canonical = L.sortOn fst . map (\(_, (x, y)) -> (y, x))

    go i labelId cl1 cl2 numLabels
      | i == n = False
      | otherwise = do
          -- Create uncompressed labels.
          let ucl1 =
                zipWith ((,) . snd) cl1 $
                  map (uncompress cl1 . neighbors ug1) v1
          let ucl2 =
                zipWith ((,) . snd) cl2 $
                  map (uncompress cl2 . neighbors ug2) v2

          -- Reduce uncompressed labels to compressed labels.
          let grp1 = group ucl1 labelId
          let grp2 = group ucl2 labelId

          let k = length grp1

          if length grp2 == k && numLabels == k
            then do
              -- Create the canonical graphs.
              let c1 = canonical grp1
              let c2 = canonical grp2
              c1 == c2
            else do
              -- Assign compressed labels.
              let cl1' = zip v1 (compress ucl1 grp1)
              let cl2' = zip v2 (compress ucl2 grp2)
              go (i + 1) (labelId + k) cl1' cl2' k

{-
Problem 86: (**) Node degree and graph coloration.

a) Write a predicate degree(Graph,Node,Deg) that determines
   the degree of a given node.

b) Write a predicate that generates a list of all nodes of a
   graph sorted according to decreasing degree.

c) Use Welch-Powell's algorithm to paint the nodes of a graph
   in such a way that adjacent nodes have different colors.

ANSWER:
https://www.youtube.com/watch?v=CQIW2mLfG04

- Sort the vertices in decreasing order of degree.
- Color the top vertex with color 1.
- For any vertex in the list that is not colored,
  and not adjacent to another vertex with color 1,
  color with 1.
- Having walked through the list, if there are any
  remaining vertices, increment color and start
  coloring again.
-}
kColor :: (Ord a) => [a] -> [Edge a] -> [(a, Int)]
kColor vs es = go vs' Map.empty 1
  where
    ug = Map.unionWith (++) (Map.fromList $ map (,[]) vs) (buildUG es)
    degrees = map (Bf.second length) $ Map.toList ug
    vs' = map fst . L.sortOn (Down . snd) $ degrees
    n = length vs

    go [] clrMap clr
      | Map.size clrMap == n = Map.toList clrMap
      | otherwise = go (filter (`Map.notMember` clrMap) vs') clrMap (clr + 1)
    go (v : xs) clrMap clr
      | canClr = go xs (Map.insert v clr clrMap) clr
      | otherwise = go xs clrMap clr
      where
        neighboringClrs = map (flip (Map.findWithDefault 0) clrMap) (neighbors ug v)
        canClr = clr `notElem` neighboringClrs

{-
Creates a spanning forest of the part of the graph reachable from the
listed vertices, obtained from a depth-first search of the graph starting
at each of the listed vertices in order.

FlexibleContexts needs to be enabled because the signature of go fixes
the 's' of (MonadState s m) to a Set, which is not allowed in Haskell 98.

  go :: S.MonadState (Set a) m => [a] -> m [[a]]
-}
dfs :: (Ord a) => Graph a -> [a] -> [[a]]
dfs g vs0 = S.evalState (go vs0) Set.empty
  where
    go [] = return []
    go (v : vs) = do
      visited <- S.get
      if v `Set.member` visited
        then go vs
        else do
          S.modify (Set.insert v)
          let adjacent = neighbors g v
          xs <- join <$> go adjacent
          ys <- go vs
          return $ (v : xs) : ys

{-
Problem 87: (**) Depth-first order graph traversal (alternative solution).

Write a predicate that generates a depth-first order graph traversal sequence.
The starting point should be specified, and the output should be a list of nodes
that are reachable from this starting point (in depth-first order).
-}
depthFirst :: (Ord a) => [a] -> [Edge a] -> a -> [a]
depthFirst vs es start = head forest
  where
    ug = Map.unionWith (++) (Map.fromList $ map (,[]) vs) (buildUG es)
    forest = dfs ug [start]

{-
Problem 88: (**) Connected components (alternative solution).

Write a predicate that splits a graph into its connected components.
-}
connectedComponents :: (Ord a) => [a] -> [Edge a] -> [[a]]
connectedComponents vs es = dfs ug $ Map.keys ug
  where
    ug = Map.unionWith (++) (Map.fromList $ map (,[]) vs) (buildUG es)

{-
Problem 89: (**) Bipartite graphs.

Write a predicate that finds out whether a given graph is bipartite.

ANSWER:
A bipartite graph is always 2-colorable, and vice-versa.
-}
bipartite :: (Ord a) => [a] -> [Edge a] -> Bool
bipartite vs = (== 2) . length . L.nub . map snd . kColor vs
