module MultiwayTree.P71Spec (spec) where

import MultiwayTree.MultiwayTree (Tree (..))
import MultiwayTree.P71
import MultiwayTree.Trees
import Test.Hspec

tree4 :: Tree Char
tree4 = Node 'b' [Node 'd' [], Node 'e' []]

spec :: Spec
spec = do
  describe "ipl" $ do
    it "calculates internal path length" $ do
      ipl tree5 `shouldBe` 9
      ipl tree4 `shouldBe` 2
