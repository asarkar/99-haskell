module List.P22 where

-- Problem 22: (*) Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range start end
  | start <= end = [start .. end]
  | otherwise = [start, start - 1 .. end]
