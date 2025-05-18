module Misc.P91 where

import qualified Data.Ix as Ix
import qualified Data.List as L
import qualified Data.Maybe as Mb
import qualified Data.Set as Set

{-
Problem 91: (**) Knight's tour.

How can a knight jump on an NxN chessboard in such a way
that it visits every square exactly once?

There are two variants of this problem:

1. find a tour ending at a particular square
2. find a circular tour, ending a knight's jump from the start
   (clearly it doesn't matter where you start, so choose (1,1))
-}
knightsTo :: Int -> (Int, Int) -> [(Int, Int)]
-- const :: a -> b -> a
-- When const f, where f is a boolean function a -> Bool ==> b -> a -> Bool
-- which means the boolean function will operate on the 2nd argument.
knightsTo n = reverse . knightsTour n (const (n * n - 1 ==))

-- https://wiki.haskell.org/The_Knights_Tour
knightsTour :: Int -> ((Int, Int) -> Int -> Bool) -> (Int, Int) -> [(Int, Int)]
knightsTour n isDone = go Set.empty
  where
    go visited pos
      | isDone pos (Set.size visited) = [pos]
      | otherwise = Mb.maybe [] (pos :) ys
      where
        xs = filter (`Set.notMember` visited) $ moves pos
        -- Warnsdorff's rule - choose the next position
        -- with the fewest onward moves.
        nexts = L.sortOn (length . moves) xs
        ys = L.find (not . null) $ map (go (Set.insert pos visited)) nexts
        moves (i, j) =
          [ (i', j')
          | (dx, dy) <- [(1, 2), (2, 1)],
            i' <- [i + dx, i - dx],
            j' <- [j + dy, j - dy],
            Ix.inRange ((1, 1), (n, n)) (i', j')
          ]

closedKnights :: Int -> [(Int, Int)]
closedKnights n = knightsTour n isDone (1, 1)
  where
    isDone pos k = pos `elem` [(2, 3), (3, 2)] && k == n * n - 1
