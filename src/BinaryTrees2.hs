{-# OPTIONS -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}

module BinaryTrees2 where

import BinaryTree (Tree (..))
import Control.Applicative ((<|>))
import qualified DList as D
import qualified Data.List.Split as LS
import Parser (Parser (..))
import qualified Parser as P

-- Problem 61: (*) Count the leaves of a binary tree.
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ l r) = case (l, r) of
  (Empty, Empty) -> 1
  _ -> countLeaves l + countLeaves r

-- Problem 61A: (*) Collect the leaves of a binary tree in a list.
leaves :: Tree a -> [a]
leaves = go []
  where
    go acc Empty = acc
    go acc (Branch x l r) = case (l, r) of
      (Empty, Empty) -> x : acc
      _ -> go (go acc r) l

-- Problem 62: (*) Collect the internal nodes of a binary tree in a list.
internals :: Tree a -> [a]
internals = go []
  where
    go acc Empty = acc
    go acc (Branch x l r) = case (l, r) of
      (Empty, Empty) -> acc
      _ -> x : go (go acc r) l

-- Problem 62B: (*) Collect the nodes at a given level in a list.
atLevel :: Tree a -> Int -> [a]
atLevel = go []
  where
    go acc Empty _ = acc
    go acc (Branch x l r) level
      | level == 1 = x : acc
      | otherwise = go (go acc r (level - 1)) l (level - 1)

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

type Pos = (Int, Int)

type AnnotatedTree a = Tree (a, Pos)

{-
Problem 64: (**) Layout algorithm for displaying trees.
In this layout strategy, the position of a node v is obtained by the following two rules:

- x(v) is equal to the position of the node v in the inorder sequence
- y(v) is equal to the depth of the node v in the tree

Write a function to annotate each node of the tree with a position,
where (1,1) in the top left corner or the rectangle bounding the drawn tree.
-}
layout :: Tree a -> AnnotatedTree a
layout = fst . go 0 1
  where
    -- Returns the modified tree and the number of nodes in it.
    go :: Int -> Int -> Tree a -> (AnnotatedTree a, Int)
    go _ _ Empty = (Empty, 0)
    go pos depth (Branch x l r) = (node, lSize + rSize + 1)
      where
        (left, lSize) = go pos (depth + 1) l
        pos' = lSize + pos + 1
        (right, rSize) = go pos' (depth + 1) r
        node = Branch (x, (pos', depth)) left right

height :: Tree a -> Int
height Empty = -1
height (Branch _ l r) = 1 + max (height l) (height r)

{-
Problem 65: (**) Layout algorithm for displaying trees (part 2).

ANSWER: In this problem, no two nodes share the same Y-coordinate.
Thus, the X-coordinate of a node is determined by the maximum
height of its subtrees. In order to avoid calculating the height of
the tree at every node, we calculate the height of the root tree first.

The nodes on the second level (children of root) are each separated
by 2 * height from the root, the nodes on the next level are
separated by half of the separation value on the level above,
and so on.

We start with the value 2 * height for the separator and halve it
each time when recurring on the children. The X-coordinate of a
node is given by the X-coordinate of its left child plus the
separation value. The X-coordinate of a right child is given by
the by the X-coordinate of its parent plus the separation value.
We also need to handle the special case for the leftmost node with
position 1.
-}
layout2 :: Tree a -> AnnotatedTree a
layout2 = fst . (go 1 1 =<< (2 *) . height)
  where
    go :: Int -> Int -> Int -> Tree a -> (AnnotatedTree a, Int)
    go _ _ _ Empty = (Empty, 0)
    go pos depth ht (Branch x l r) = (node, pos')
      where
        depth' = depth + 1
        ht' = ht `div` 2
        (left, lPos) = go (pos - ht) depth' ht' l
        pos' = if lPos > 0 then lPos + ht else max pos 1
        (right, _) = go (pos' + ht) depth' ht' r
        node = Branch (x, (pos', depth)) left right

{-
Problem 66: (***) Layout algorithm for displaying trees (part 3).

The method yields a very compact layout while maintaining a
certain symmetry in every node. Find out the rules and write
the corresponding predicate.
Hint: Consider the horizontal distance between a node and its
successor nodes. How tight can you pack together two subtrees
to construct the combined binary tree?

ANSWER: We have to place the left and right subtrees so that
two nodes at the same level may not overlap, and that the
parent is evenly spaced between its children.

TODO.
-}

{-
Problem 67A: (**) A string representation of binary trees.
Write a predicate which generates this string representation.
Then write a predicate which does this inverse; i.e. given the
string representation, construct the tree in the usual form.
Finally, combine the two predicates in a single predicate
which can be used in both directions.
-}

type TreeParser = Parser (Tree Char)

branch :: TreeParser
branch = do
  x <- P.letter
  left <- P.open *> binaryTree <* P.comma
  right <- binaryTree <* P.close
  return $ Branch x left right

emptyTree :: TreeParser
emptyTree = pure Empty

singleton :: TreeParser
singleton = do
  x <- P.letter
  return $ Branch x Empty Empty

binaryTree :: TreeParser
binaryTree = branch <|> singleton <|> emptyTree

stringToTree :: String -> Tree Char
stringToTree = fst . head . P.parse binaryTree

treeToString :: Tree Char -> String
treeToString = D.toList . go D.empty
  where
    go acc Empty = acc
    go acc (Branch x Empty Empty) = acc D.++ D.singleton x
    go acc (Branch x l r) =
      acc
        D.++ D.singleton x
        D.++ D.singleton '('
        D.++ go D.empty l
        D.++ D.singleton ','
        D.++ go D.empty r
        D.++ D.singleton ')'

{-
Problem 68: (**) Preorder and inorder sequences of binary trees.

a) Write predicates preorder and inorder that construct the
   preorder and inorder sequence of a given binary tree,
   respectively.

b) Can you use preorder from problem part a) in the reverse
   direction; i.e. given a preorder sequence, construct a
   corresponding tree? If not, make the necessary arrangements.

c) If both the preorder sequence and the inorder sequence of
   the nodes of a binary tree are given, then the tree is
   determined unambiguously.
   Write a predicate pre_in_tree that does the job.
-}
treeToPreorder :: Tree Char -> String
treeToPreorder = go []
  where
    go acc Empty = acc
    go acc (Branch x l r) = x : go (go acc r) l

treeToInorder :: Tree Char -> String
treeToInorder = go []
  where
    go acc Empty = acc
    go acc (Branch x l r) = go (x : go acc r) l

{-
ANSWER: We take each element from the preorder and find it in the inorder.
This is the root value, the left sequence is the left tree, and the right
sequence is the right tree. We recurse on the left and the right subtrees.

In addition to returning the node, we also return the preorder string that
is yet to be processed.
-}
preInTree :: String -> String -> Tree Char
preInTree pre = fst . build pre
  where
    build :: String -> String -> (Tree Char, String)
    build po [] = (Empty, po)
    build (x : xs) [_] = (Branch x Empty Empty, xs)
    build (x : xs) io = (Branch x left right, zs)
      where
        [io', io''] = LS.splitOn [x] io
        (left, ys) = build xs io'
        (right, zs) = build ys io''

{-
Problem 69: (**) Dotstring representation of binary trees.

First, try to establish a syntax (BNF or syntax diagrams)
and then write a predicate tree_dotstring which does the
conversion in both directions. Use difference lists.

ANSWER:
tree ::= empty | branch
branch ::= letter tree tree
empty ::= '.'
letter ::= 'a' ... 'z'
-}
tree2ds :: Tree Char -> String
tree2ds = D.toList . go
  where
    go Empty = D.singleton '.'
    go (Branch x l r) = D.singleton x D.++ go l D.++ go r

ds2tree :: String -> Tree Char
ds2tree = fst . go
  where
    go [] = (Empty, "")
    go ('.' : xs) = (Empty, xs)
    go (x : xs) = (Branch x left right, zs)
      where
        (left, ys) = go xs
        (right, zs) = go ys
