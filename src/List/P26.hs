module List.P26 where

import qualified Data.List as L

-- Problem 26: (**) Generate the combinations of K distinct
-- objects chosen from the N elements of a list.
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = return []
combinations n xs = do
  y : ys <- L.tails xs
  zs <- combinations (n - 1) ys
  return (y : zs)
