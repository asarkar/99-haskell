module Graph.P88Spec where

import Graph.P88
import Test.Hspec

spec :: Spec
spec = do
  describe "connectedComponents" $ do
    it "splits a graph into its connected components" $ do
      let v1 = [1 .. 7 :: Int]
      let e1 = [(1, 2), (2, 3), (1, 4), (3, 4), (5, 2), (5, 4), (6, 7)]
      connectedComponents v1 e1 `shouldMatchList` [[1, 2, 3, 4, 5], [6, 7]]
