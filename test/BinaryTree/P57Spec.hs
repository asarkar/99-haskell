module BinaryTree.P57Spec (spec) where

import BinaryTree.BinaryTree (fromList)
import BinaryTree.P56
import BinaryTree.P57
import Test.Hspec

spec :: Spec
spec = do
  describe "construct" $ do
    it "builds a BST from a list of integers" $ do
      let t1 = fromList read ["3", "2", "5", "1", "null", "null", "7"]

      construct [3, 2, 5, 7, 1 :: Int] `shouldBe` t1

      (symmetric . construct $ [5, 3, 18, 1, 4, 12, 21 :: Int]) `shouldBe` True
      (symmetric . construct $ [3, 2, 5, 7, 1 :: Int]) `shouldBe` True
