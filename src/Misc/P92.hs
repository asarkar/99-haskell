{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wno-incomplete-uni-patterns #-}

module Misc.P92 (vonKochLabeling) where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down (..))
import Data.Set (Set)
import qualified Data.Set as Set

{-
Problem 92: (***) Von Koch's conjecture.

It has been conjectured that if graph G is a tree with n vertices, and thus n-1 edges,
then there is a graceful labeling of the tree. This means that there is a way to label
each vertex with an integer from 1 to n such that there is an edge whose difference in
vertex labels is between 1 and n-1.
There is no known counterexample, but neither is it proven that this is true for all trees.

Example:
n=7

┌──┐  6                 2    ┌──┐  1   ┌──┐
│1 ├────────┐       ┌────────┤5 ├──────┤4 │
└──┘        │       │        └──┘      └──┘
          ┌─┴┐  4  ┌┴─┐
          │ 7├─────┤3 │
          └─┬┘     └┬─┘
┌──┐  5     │       │   3    ┌──┐
│2 ├────────┘       └────────┤6 │
└──┘                         └──┘

ANSWER: The requirement is that the edge and vertex labels must be unique, and that the vertex
labels are in the closed range [1,n], and the edge labels are in the closed range [1,n-1].
See test/resources/92.jpg for examples.

This problem is also known as Graceful labeling (https://en.wikipedia.org/wiki/Graceful_labeling).
It can be solved by DFS with backtracking. We try vertices with higher degrees first
as might prune the search space better.
-}

type Edge = (Int, Int)

type Graph = [Edge]

data Partial = Partial
  { -- | All edges
    edges :: Graph,
    -- | Vertices yet to be labeled; sorted in desc order of degree
    remainingVertices :: [Int],
    -- | Vertex labels yet to be used
    remainingLabels :: [Int],
    -- | Used edge labels; two edges can't have the same label
    edgeLabels :: Set Int,
    -- | Vertex -> Label assignments
    vertexLabels :: Map Int Int,
    numVertices :: Int
  }

vonKochLabeling :: Graph -> Map Int Int
vonKochLabeling edges = labelVertex Partial {..}
  where
    deg = degrees edges
    -- Trying vertices with higher degrees first might constrain labeling more,
    -- i.e, prune the search space.
    remainingVertices =
      (L.sortOn (Down . (deg Map.!)) . L.nub . concat)
        [[u, v] | (u, v) <- edges]
    numVertices = length remainingVertices
    remainingLabels = [1 .. numVertices]
    edgeLabels = Set.empty
    vertexLabels = Map.empty

degrees :: Graph -> Map Int Int
degrees = foldr (\(u, v) deg -> addNode v (addNode u deg)) Map.empty
  where
    addNode n = Map.insertWith (+) n 1

neighbors :: Graph -> Int -> [Int]
neighbors edges u = [if a == u then b else a | (a, b) <- edges, a == u || b == u]

labelVertex :: Partial -> Map Int Int
labelVertex
  p@Partial
    { edges,
      remainingVertices,
      remainingLabels,
      edgeLabels,
      vertexLabels,
      numVertices
    }
    | null remainingLabels && length vertexLabels == numVertices = vertexLabels
    | null remainingLabels = Map.empty
    | otherwise = do
        let (l : ls) = remainingLabels
        let (u : us) = remainingVertices
        let vs = neighbors edges u
        let diffs =
              Set.fromList
                ( [ abs (l - (vertexLabels Map.! v))
                  | v <- vs,
                    v `Map.member` vertexLabels
                  ]
                )
        let next = labelVertex (p {remainingLabels = ls})
        -- If any of the edgeLabels are used already, then this label is no good, try another label
        if not (Set.disjoint diffs edgeLabels)
          then next
          -- Label is unused, assign, and recurse with rest of the vertices and labels.
          else do
            let vl' = Map.insert u l vertexLabels
            let usedLabels = Set.fromList [v | (_, v) <- Map.assocs vl']
            -- Compute fresh, not from `remainingLabels`; remainingLabels is used to indicate
            -- a failed assignment; i.e. when a label is chosen that had been used before.
            let rl' = [v | v <- [1 .. numVertices], v `Set.notMember` usedLabels]
            let el' = Set.union edgeLabels diffs
            let labels =
                  labelVertex
                    ( p
                        { remainingVertices = us,
                          remainingLabels = rl',
                          edgeLabels = el',
                          vertexLabels = vl'
                        }
                    )
            if Map.null labels then next else labels
