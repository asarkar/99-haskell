module Monad.P77 where

import qualified Control.Monad as M

{-
Problem 77: (*) List monad.

Using the list monad, implement a function which returns all the one-dimensional random walk
paths with n steps. Starting from position 0, each step can change positions by -1, 0, or 1.
Each path will be a list of positions starting from 0.

ANSWER:
ReplicateM creates the cross-product of the given list n times.

Example:
  n=2, [[-1,-1], [-1,0], [-1,1], [0,-1], [0,0], [0,1], [1,-1], [1,0], [1,1]].
  Each of these represents the next step taken, so [-1,-1] means 2 steps from 0,
  0, -1 and -1. The position at each step is given by the sum of itself with
  the previous position, i.e. the cumulative sum.
  So, 0, -1 and -1 ==> 0, -1, -2.
-}

randomWalkPaths :: Int -> [[Int]]
randomWalkPaths 0 = [[0]]
randomWalkPaths n = map (scanl (+) 0) $ M.replicateM n [-1, 0, 1]
