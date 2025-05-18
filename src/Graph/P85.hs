module Graph.P85 where

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Maybe as Mb
import Graph.Graph

{-
Problem 85: (**) Graph isomorphism.

Two graphs G1(N1,E1) and G2(N2,E2) are isomorphic if there is a bijection
f: N1 -> N2 such that for any nodes X,Y of N1, X and Y are adjacent
if and only if f(X) and f(Y) are adjacent.

Write a predicate that determines whether two graphs are isomorphic.

ANSWER:
We apply the Weisfeiler Leman graph isomorphism test.

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

    -- Finds old label.
    label :: (Eq a) => [(a, Int)] -> a -> Int
    label cl = Mb.fromJust . flip L.lookup cl
    -- Given the neighbors and their compressed labels,
    -- computes new uncompressed label for this vertex.
    uncompress :: (Eq a) => [(a, Int)] -> [a] -> [Int]
    uncompress cl = L.sort . map (label cl)
    -- Groups uncompressed labels, and
    -- assigns a label to each group.
    group :: [(Int, [Int])] -> Int -> [((Int, [Int]), (Int, Int))]
    group ucl labelId =
      zipWith
        (\xs k -> (head xs, (length xs, k)))
        (L.group $ L.sort ucl)
        [labelId + 1 ..]
    -- Replaces each uncompressed group with its compressed label.
    compress :: [(Int, [Int])] -> [((Int, [Int]), (Int, Int))] -> [Int]
    compress ucl groups = map (snd . Mb.fromJust . flip L.lookup groups) ucl
    -- Reduces the graph into canonical form.
    canonical :: [((Int, [Int]), (Int, Int))] -> [(Int, Int)]
    canonical = L.sortOn fst . map (\(_, (x, y)) -> (y, x))

    -- go :: Int -> Int -> [(a, Int)] -> [(b, Int)] -> Int -> Bool
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
