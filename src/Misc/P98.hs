module Misc.P98 where

{-
Problem 98: (***) Nonograms.

The puzzle goes like this: Essentially, each row and column of a rectangular bitmap
is annotated with the respective lengths of its distinct strings of occupied cells.
The person who solves the puzzle must complete the bitmap given only these lengths.

Example:

  Problem:                    Solution:

  |_|_|_|_|_|_|_|_| 3         |_|X|X|X|_|_|_|_| 3
  |_|_|_|_|_|_|_|_| 2 1       |X|X|_|X|_|_|_|_| 2 1
  |_|_|_|_|_|_|_|_| 3 2       |_|X|X|X|_|_|X|X| 3 2
  |_|_|_|_|_|_|_|_| 2 2       |_|_|X|X|_|_|X|X| 2 2
  |_|_|_|_|_|_|_|_| 6         |_|_|X|X|X|X|X|X| 6
  |_|_|_|_|_|_|_|_| 1 5       |X|_|X|X|X|X|X|_| 1 5
  |_|_|_|_|_|_|_|_| 6         |X|X|X|X|X|X|_|_| 6
  |_|_|_|_|_|_|_|_| 1         |_|_|_|_|X|_|_|_| 1
  |_|_|_|_|_|_|_|_| 2         |_|_|_|X|X|_|_|_| 2
   1 3 1 7 5 3 4 3             1 3 1 7 5 3 4 3
   2 1 5 1                     2 1 5 1

3 against the 1st row means there should be 3 consecutive 'X's in the solution.
2 1 against the 2nd row means there should be 2 consecutive 'X's, followed by
_at least_ one space, then another 'X'.

The numbers at the bottom are the constraints for the corresponding columns.

ANSWER: TODO.
-}
