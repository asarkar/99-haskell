module List.P21 where

import qualified Data.List as L

-- Problem 21: (*) Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n
  | n > 0 = left ++ [x] ++ right
  | otherwise = error "invalid position"
  where
    (left, right) = L.splitAt (n - 1) xs
