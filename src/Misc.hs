module Misc (queens, knightsTour, knightsTo, closedKnights) where

import qualified Control.Monad as M
import qualified Data.Ix as Ix
import qualified Data.List as L
import qualified Data.Maybe as Mb
import qualified Data.Set as Set

{-
Problem 90: (**) Eight queens problem.

The objective is to place eight queens on a chessboard so that
no two queens are attacking each other; i.e., no two queens
are in the same row, the same column, or on the same diagonal.

ANSWER:
We start from the nth column and move left. Since calculating
the we length of a list is a linear time opertation in Haskell,
also keep track of the number of queens placed so far. The
rows are index by the columns, so [1, 3] means two queens
placed at (r: 1, c: 7) and (r: 3: c: 8).

At the ith iteration, where 0 <= i < n, we are trying to find a row
for the column n-i. The columns n-i+1 through n have been filled,
and the corresponding rows are available in the list 'rows'.

We observe that a (row, col) is under attack from one of the queens
if a queen is present in the same row (in the list rows), or if
(row, col) is on the same diagonal or anti-diagonal. If row - queen_row
is equal to col - queen_col, then (row, col) is under attack diagonally.
-}
queens :: Int -> [[Int]]
queens n = go 0 []
  where
    go :: Int -> [Int] -> [[Int]]
    go numPlaced rows
      | numPlaced == n = return rows
      | otherwise = do
          row <- [1 .. n]
          _ <- M.guard (canPlace numPlaced rows row)
          go (numPlaced + 1) (row : rows)

    canPlace numPlaced rows row
      | row `elem` rows = False
      | otherwise = all (\(r, c) -> abs (row - r) /= abs (col - c)) $ zip rows cols
      where
        col = n - numPlaced
        cols = [col + 1 .. n]

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

closedKnights :: Int -> [(Int, Int)]
closedKnights n = knightsTour n isDone (1, 1)
  where
    isDone pos k = pos `elem` [(2, 3), (3, 2)] && k == n * n - 1

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

ANSWER: TODO.
-}

{-
Problem 93: (***) An arithmetic puzzle.

Given a list of positive integer numbers, find a correct way of inserting
the arithmetic signs such that the result is a correct equation.

For example, with the list of numbers [2,3,5,7,11], we can form the equations
2-3+5+7=11 or 2=(3*5+7)/11.

The arithmetic signs to insert are:

+ : addition
- : subtraction
\* : multiplication
/ : division
= : equality
(, ) : parentheses

Arithmetic operations are only binary, e.g., -4 should not be included.
Division should be interpreted as operating on rationals, e.g., 3/5=6/10
but 3/5≠0, and division by zero should be avoided.
Parentheses should be inserted only when the default precedence rules
need to be overridden. Equality should be inserted exactly once.

ANSWER: TODO.
-}

{-
Problem 94: (***) Generate K-regular simple graphs with N nodes.

In a k-regular graph, all vertexes have a degree of k, i.e.,
the number of edges incident in each vertex is k.

How many non-isomorphic 3-regular graphs with 6 vertexes are there?

ANSWER: TODO.
-}
