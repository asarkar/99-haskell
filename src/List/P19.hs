module List.P19 where

import List.P17

-- Problem 19: (**) Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate xs k = right ++ left
  where
    n = k `mod` length xs
    (left, right) = split xs n
