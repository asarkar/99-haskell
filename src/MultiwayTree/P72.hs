module MultiwayTree.P72 where

import MultiwayTree.MultiwayTree (Tree (..))

-- Problem 72: (*) Construct bottom-up order sequence of multiway tree nodes.
bottomUp :: Tree a -> [a]
bottomUp = go []
  where
    go acc (Node x forest) = foldr (flip go) (x : acc) forest
