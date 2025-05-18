module Misc.P97 where

import qualified Control.Monad as M
import qualified Data.Maybe as Mb
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

{-
Problem 97: (**) Sudoku.

ANSWER:
Backtracking. The candidates for a cell are the numbers _not_
present in the same row, column, and 3x3 box. Finding the
numbers in the same row and column are easy. For the box,
we first first its top-left coordinate, then iterate
left-to-right and top-to-bottom.

We start from the top-left cell, and for every cell containing
a zero, fill it with one of the candidates, and move on to the
next empty cell. If at any point, there are no candidates
available for a cell, we backtrack, and try the next candidate
for the previous cell.
-}
sudoku :: [[Int]] -> [[Int]]
sudoku grid = (V.toList . V.map VU.toList) (Mb.fromJust soln)
  where
    soln = solve 0 0 initialBoard
    initialBoard = V.fromList $ map VU.fromList grid
    row = flip (V.!)
    col n = V.map (VU.! n)
    box r c board = [get r' c' board | r' <- [x .. x + 2], c' <- [y .. y + 2]]
      where
        x = 3 * (r `div` 3)
        y = 3 * (c `div` 3)
    isValid r c val board =
      val `VU.notElem` row r board
        && val `V.notElem` col c board
        && val `notElem` box r c board
    candidates r c board = [x | x <- [1 .. 9], isValid r c x board]
    get r c board = (board V.! r) VU.! c
    set r c val board =
      V.update board $
        V.singleton
          ( r,
            VU.update (row r board) $
              VU.singleton (c, val)
          )
    solve r c board
      | r > 8 = Just board
      | c > 8 = solve (r + 1) 0 board
      | get r c board /= 0 = solve r (c + 1) board
      | otherwise = M.msum $ do
          x <- candidates r c board
          let b = set r c x board
          return $ solve r (c + 1) b
