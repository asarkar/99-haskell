module BinaryTree.P59Spec (spec) where

import BinaryTree.BinaryTree (fromList)
import BinaryTree.P59
import qualified Control.Monad as M
import Test.Hspec

spec :: Spec
spec = do
  describe "hbalTree" $ do
    it "constructs all height-balanced binary trees with the given maximum height" $ do
      let t1 = fromList head ["x", "x", "x", "null", "null", "null", "x"]
      let t2 = fromList head ["x", "x", "x", "null", "null", "x"]
      let t3 = fromList head ["x", "x", "x", "null", "null", "x", "x"]
      let t4 = fromList head ["x", "x", "x", "null", "x"]

      let trees = hbalTree 'x' 3
      M.forM_ [t1, t2, t3, t4] $ \t -> do
        trees `shouldSatisfy` elem t
