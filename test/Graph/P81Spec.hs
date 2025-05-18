module Graph.P81Spec where

import Graph.P81
import Test.Hspec

spec :: Spec
spec = do
  describe "paths" $ do
    it "returns all ayclic paths between the source and destination" $ do
      let edges = [(1, 2), (2, 3), (1, 3), (3, 4), (4, 2), (5, 6)]

      paths 1 4 edges
        `shouldMatchList` [[1 :: Int, 2, 4], [1, 2, 3, 4], [1, 3, 2, 4], [1, 3, 4]]

      paths 2 6 edges `shouldBe` []
