module Graph.P89Spec where

import Graph.P89
import Test.Hspec

spec :: Spec
spec = do
  describe "bipartite" $ do
    it "determines whether a given graph is bipartite" $ do
      let v1 = [1 .. 5 :: Int]
      let e1 = [(1, 2), (2, 3), (1, 4), (3, 4), (5, 2), (5, 4)]
      bipartite v1 e1 `shouldBe` True

      let e2 = [(1, 2), (2, 3), (1, 4), (3, 4), (5, 2), (5, 4), (1, 3)]
      bipartite v1 e2 `shouldBe` False
