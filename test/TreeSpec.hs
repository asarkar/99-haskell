{-# OPTIONS -Wno-orphans #-}

module TreeSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Tree (Tree (..), fromList, toList)

leaf :: a -> Tree a
leaf x = Node x Empty Empty

-- https://www.dcc.fc.up.pt/~pbv/aulas/tapf/handouts/quickcheck.html
genTree :: (Arbitrary a) => Int -> Gen (Tree a)
genTree 0 = return Empty
genTree size = frequency [(4, genNode), (1, return Empty)]
  where
    genNode = do
      let size' = size `div` 2
      v <- arbitrary
      l <- genTree size'
      r <- genTree size'
      return $ Node v l r

-- https://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck-Arbitrary.html
shrinkTree :: (Arbitrary a) => Tree a -> [Tree a]
shrinkTree Empty = []
shrinkTree (Node x l r) =
  -- shrink Branch to Nil
  [Empty]
    ++
    -- shrink to subterms
    [l, r]
    ++
    -- recursively shrink subterms
    [Node x' l' r' | (x', l', r') <- shrink (x, l, r)]

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = chooseInt (0, 100) >>= genTree
  shrink = shrinkTree

spec :: Spec
spec = do
  describe "Tree" $ do
    let t = Node 5 (Node 3 (Node 2 (leaf 1) Empty) (leaf 4)) (leaf 6) :: Tree Int
    let ts = ["5", "3", "6", "2", "4", "null", "null", "1"]

    it "can be built from a list of strings" $ do
      (fromList read ts :: Tree Int) `shouldBe` t
      (fromList read [] :: Tree Int) `shouldBe` Empty

    it "can be converted to a list of strings" $ do
      toList t `shouldBe` ts
      toList (Empty :: Tree Int) `shouldBe` []

    it "fromList is the inverse of toList" $
      within 1000000 $
        \t' -> fromList read (toList (t' :: Tree Int)) `shouldBe` t'
