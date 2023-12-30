module BinaryTrees where

import BinaryTree (Tree (..))

-- Problem 54A: (*) Check whether a given term represents a binary tree.
-- ANSWER: Creating an invalid tree is not possible in Haskell.

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

{-
Problem 56: (**) Symmetric binary trees.

A binary tree is symmetric if you can draw a vertical line
through the root node and then the right subtree is the
mirror image of the left subtree.
-}
symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

mirror :: Tree a -> Tree a -> Bool
mirror t1 t2 = case (t1, t2) of
  (Empty, Empty) -> True
  (Empty, _) -> False
  (_, Empty) -> False
  (Branch _ l1 r1, Branch _ l2 r2) -> mirror l1 r2 && mirror r1 l2

-- Inserts 'a' into a BST.
add :: (Ord a) => a -> Tree a -> Tree a
add x Empty = Branch x Empty Empty
add x t@(Branch y l r) = case compare x y of
  LT -> Branch y (add x l) r
  GT -> Branch y l (add x r)
  EQ -> t

{-
Problem 57: (**) Binary search trees.
-}
construct :: (Ord a) => [a] -> Tree a
construct = foldl (flip add) Empty

{-
Problem 58: (**) Generate-and-test paradigm.

Apply the generate-and-test paradigm to construct all symmetric,
completely balanced binary trees with a given number of nodes.
-}
symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree

{-
Problem 59: (***) Construct height-balanced binary trees.

Construct a list of all height-balanced binary trees with
the given element and the given maximum height.
-}
hbalTree :: a -> Int -> [Tree a]
hbalTree x n
  | n == 0 = []
  | n == 1 = [singleton x]
  | n == 2 =
      [ Branch x (singleton x) Empty,
        Branch x Empty (singleton x),
        Branch x (singleton x) (singleton x)
      ]
  | otherwise = do
      {-
      Either the subtrees are of equal height,
      or differ by 1. Generate those combinations.
      -}
      xs <- hbalTree x (n - 1)
      ys <- hbalTree x (n - 2)
      [Branch x xs ys, Branch x ys xs, Branch x xs xs]

{-
Problem 60: (***) Construct height-balanced binary trees with a given number of nodes.

ANSWER: TODO.
-}
