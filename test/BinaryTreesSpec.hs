module BinaryTreesSpec (spec) where

import BinaryTrees
import qualified Control.Monad as M
import Test.Hspec
import Tree (Tree (..), fromList)

spec :: Spec
spec = do
  describe "cbalTree" $ do
    it "constructs completely balanced binary trees" $ do
      let trees =
            map
              (fromList head)
              [ ["x", "x", "x", "null", "null", "x"],
                ["x", "x", "x", "x"],
                ["x", "x", "x", "null", "null", "null", "x"],
                ["x", "x", "x", "null", "x"]
              ]

      (cbalTree 4 :: [Tree Char]) `shouldMatchList` trees

  describe "symmetric" $ do
    it "checks if a tree is symmetric" $ do
      let t1 = fromList head ["x", "x"]
      symmetric t1 `shouldBe` False

      let t2 = fromList head ["x", "x", "x"]
      symmetric t2 `shouldBe` True

  describe "construct" $ do
    it "builds a BST from a list of integers" $ do
      let t1 = fromList read ["3", "2", "5", "1", "null", "null", "7"]

      construct [3, 2, 5, 7, 1 :: Int] `shouldBe` t1

      (symmetric . construct $ [5, 3, 18, 1, 4, 12, 21 :: Int]) `shouldBe` True
      (symmetric . construct $ [3, 2, 5, 7, 1 :: Int]) `shouldBe` True

  describe "symCbalTrees" $ do
    it "constructs completely balanced symmetric binary trees" $ do
      let trees =
            map
              (fromList head)
              [ ["x", "x", "x", "null", "x", "x"],
                ["x", "x", "x", "x", "null", "null", "x"]
              ]

      symCbalTrees 5 `shouldMatchList` trees

  describe "hbalTree" $ do
    it "constructs all height-balanced binary trees with the given maximum height" $ do
      let t1 = fromList head ["x", "x", "x", "null", "null", "null", "x"]
      let t2 = fromList head ["x", "x", "x", "null", "null", "x"]
      let t3 = fromList head ["x", "x", "x", "null", "null", "x", "x"]
      let t4 = fromList head ["x", "x", "x", "null", "x"]

      let trees = hbalTree 'x' 3
      M.forM_ [t1, t2, t3, t4] $ \t -> do
        trees `shouldSatisfy` elem t
