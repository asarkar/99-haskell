module Graph.P87Spec where

import Graph.P87
import Test.Hspec

spec :: Spec
spec = do
  describe "depthFirst" $ do
    it "generates a list of nodes that are reachable from the starting point" $ do
      let v1 = [1 .. 7 :: Int]
      let e1 = [(1, 2), (2, 3), (1, 4), (3, 4), (5, 2), (5, 4), (6, 7)]
      depthFirst v1 e1 1 `shouldMatchList` [1 .. 5]
