-- This is needed to convince the compiler that an
-- Arbitrary (Tree a) will be supplied in due time.
{-# LANGUAGE FlexibleContexts #-}

module BinaryTree.GenBinaryTree where

import BinaryTree.BinaryTree (Tree (..))
import Test.QuickCheck

-- https://www.dcc.fc.up.pt/~pbv/aulas/tapf/handouts/quickcheck.html
genTree :: Gen a -> Int -> Gen (Tree a)
genTree _ 0 = return Empty
genTree g n = frequency [(4, genNode), (1, return Empty)]
  where
    genNode = do
      let n' = n `div` 2
      x <- g
      l <- genTree g n'
      r <- genTree g n'
      return $ Branch x l r

-- https://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck-Arbitrary.html
shrinkTree :: (Arbitrary a, Arbitrary (Tree a)) => Tree a -> [Tree a]
shrinkTree Empty = []
shrinkTree (Branch x l r) =
  -- shrink Branch to Nil
  [Empty]
    ++
    -- shrink to subterms
    [l, r]
    ++
    -- recursively shrink subterms
    [Branch x' l' r' | (x', l', r') <- shrink (x, l, r)]
