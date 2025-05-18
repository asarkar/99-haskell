module List.P18 where

import List.P17

-- Problem 18: (**) Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice xs start end
  | null right = right
  | otherwise = take (end - start + 1) right
  where
    (_, right) = split xs (start - 1)
