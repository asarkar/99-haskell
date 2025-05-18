module MultiwayTree.P70cSpec (spec) where

import MultiwayTree.MultiwayTree (Tree (..))
import MultiwayTree.P70c
import MultiwayTree.Trees
import Test.Hspec

spec :: Spec
spec = do
  describe "nnodes" $ do
    it "counts the number of nodes" $ do
      nnodes (Node 'a' [Node 'b' []]) `shouldBe` 2
      nnodes t1 `shouldBe` 7
