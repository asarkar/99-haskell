module Graph.P82Spec where

import Graph.P82
import Test.Hspec
import Prelude hiding (cycle)

spec :: Spec
spec = do
  describe "cycle" $ do
    it "finds all cycles starting at a given node" $ do
      let edges = [(1, 2), (2, 3), (2, 4), (1, 3), (3, 4), (4, 2), (5, 6)]

      cycle 1 edges
        `shouldMatchList` [[1 :: Int, 2, 3, 1], [1, 2, 4, 3, 1], [1, 3, 2, 1], [1, 3, 4, 2, 1]]
