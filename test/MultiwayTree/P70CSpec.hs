module MultiwayTree.P70CSpec (spec) where

import MultiwayTree.MultiwayTree (Tree (..))
import MultiwayTree.P70C
import MultiwayTree.Trees
import Test.Hspec

spec :: Spec
spec = do
  describe "nnodes" $ do
    it "counts the number of nodes" $ do
      nnodes (Node 'a' [Node 'b' []]) `shouldBe` 2
      nnodes t1 `shouldBe` 7
