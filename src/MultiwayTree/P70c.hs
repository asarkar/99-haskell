{-# LANGUAGE DerivingStrategies #-}

module MultiwayTree.P70C where

import MultiwayTree.MultiwayTree (Tree (..))

-- Problem 70C: (*) Count the nodes of a multiway tree.
nnodes :: Tree a -> Int
nnodes (Node _ forest) = 1 + foldr ((+) . nnodes) 0 forest
