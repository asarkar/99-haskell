module BinaryTree.P55 (cbalTree) where

import BinaryTree.BinaryTree (Tree (..))

{-
Problem 55: (**) Construct completely balanced binary trees.

In a completely balanced binary tree, the following property holds for every node:
The number of nodes in its left subtree and the number of nodes in its right subtree
are almost equal, which means their difference is not greater than one.
-}
cbalTree :: Int -> [Tree Char]
cbalTree = map fst . build
  where
    build :: Int -> [(Tree Char, Int)]
    build n
      | n == 0 = []
      | n == 1 = [(singleton 'x', 1)]
      | n == 2 =
          [ (Branch 'x' (singleton 'x') Empty, 2),
            (Branch 'x' Empty (singleton 'x'), 2)
          ]
      | otherwise = do
          let k = (n - 1) `div` 2
          {-
          We can take the cartesian product of 2 lists
          in a variety of ways.
            1. Monad -- liftM2 (,)
            2. sequence -- generates a list of lists
            3. Applicative -- (,) <$> xs <*> ys
            4. List comprehension -- [(x, y) | x <- xs, y <- yy]
            5. do notation
            6. Bind -- xs >>= (\x -> ys >>= (\y -> (x, y)))
            7. Bind pointfree (ys >>=) . (,) =<< xs
            8. Using MonadPlus -- xs `mplus` ys
          -}
          (l, i) <- build k
          (r, j) <- build (n - k - 1)
          let x = i + j + 1
          {-
          Create new trees combining the left and right subtrees.
          If they have the same height, then swapping them produces
          the same tree, so, we check in order to avoid generating
          duplicates.
          -}
          (Branch 'x' l r, x) : [(Branch 'x' r l, x) | abs (i - j) == 1]

singleton :: a -> Tree a
singleton x = Branch x Empty Empty
