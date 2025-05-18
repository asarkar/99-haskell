{-# OPTIONS -Wno-incomplete-uni-patterns #-}

module List.P24 where

import qualified Data.List as L
import qualified System.Random as R

-- Problem 24: (*) Draw N different random numbers from the set 1..M.
-- Note: The selected elements are unique.
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = randomElems n m [1 .. m]

randomElems :: Int -> Int -> [a] -> IO [a]
randomElems n m xs
  | n <= 0 = pure []
  | n > m = pure xs
  | otherwise = do
      k <- R.randomRIO (0, m - 1)
      let (left, x : right) = L.splitAt (k - 1) xs
      (x :) <$> randomElems (n - 1) (m - 1) (left ++ right)
