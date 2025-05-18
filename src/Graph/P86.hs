module Graph.P86 where

import qualified Data.Bifunctor as Bf
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Ord (Down (..))
import Graph.Graph

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
      -- Try another color for the remaining vertices.
      | otherwise = go (filter (`Map.notMember` clrMap) vs') clrMap (clr + 1)
    -- Try to assign color clr to each vertex, making sure
    -- no two adjacent vertices end up with the same color.
    go (v : xs) clrMap clr
      | canClr = go xs (Map.insert v clr clrMap) clr
      | otherwise = go xs clrMap clr
      where
        neighboringClrs = map (flip (Map.findWithDefault 0) clrMap) (neighbors ug v)
        canClr = clr `notElem` neighboringClrs
