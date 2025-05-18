module List.P20 where

import List.P17

-- Problem 20: (*) Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs
  | null right = error "n too large"
  | otherwise = (head right, left ++ tail right)
  where
    (left, right) = split xs (n - 1)
