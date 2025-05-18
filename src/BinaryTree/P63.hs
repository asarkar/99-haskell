module BinaryTree.P63 where

import BinaryTree.BinaryTree (Tree (..))

-- Problem 63: (**) Construct a complete binary tree.
{-
ANSWER:
Considering the height H of the tree as the number of edges on
the longest path from the root to a leaf, the totalnumber of
nodes up to the penultimate level is given by 2^H - 1.
We start with the remaining nodes on the level H, and
associate them pairwise with the nodes on the level H - 1.

Then these nodes become the input for the next iteration.
-}
-- completeBinaryTree :: Int -> Tree Char
-- completeBinaryTree n
--   | n == 1 = singleton
--   | otherwise = head $ foldl assoc nodes [ht - 1, ht - 2 .. 0]
--   where
--     ht = floor $ logBase (2 :: Float) (fromIntegral n) :: Int
--     nodes = replicate (n - (2 ^ ht - 1)) singleton
--     singleton = Branch 'x' Empty Empty

--     assoc children level = take (2 ^ level) $ parents ++ repeat singleton
--       where
--         parents = merge <$> LS.chunksOf 2 children

--     merge [l, r] = Branch 'x' l r
--     merge xs = Branch 'x' (head xs) Empty

-- Problem 63: Construct a complete binary tree.
--
-- ANSWER: Alternative (simpler) implementation.
completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = go 1
  where
    go i
      | i <= n = Branch 'x' (go (2 * i)) (go (2 * i + 1))
      | otherwise = Empty
