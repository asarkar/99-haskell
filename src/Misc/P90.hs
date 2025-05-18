module Misc.P90 (queens) where

import qualified Control.Monad as M

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
