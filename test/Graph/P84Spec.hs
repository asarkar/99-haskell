module Graph.P84Spec where

import qualified Data.List as L
import Graph.P84
import Test.Hspec

spec :: Spec
spec = do
  describe "prim" $ do
    it "constructs all minimum spanning trees" $ do
      let edges =
            [ ('a', 'b', 5),
              ('a', 'd', 3),
              ('b', 'c', 2),
              ('b', 'e', 4),
              ('c', 'e', 6),
              ('d', 'e', 7),
              ('d', 'f', 4),
              ('d', 'g', 3),
              ('e', 'h', 5),
              ('f', 'g', 4),
              ('g', 'h', 1)
            ]

      let findEdge (u, v, _) = L.find (\(x, y, _) -> (x == u && y == v) || (y == u) && x == v) edges
      let cost = maybe 0 (\(_, _, c) -> c) . findEdge
      let numVertices = length . L.nub . concatMap (\(u, v, _) -> [u, v])
      let mst = prim edges

      numVertices mst `shouldBe` numVertices edges
      let total = foldl ((. cost) . (+)) 0 mst
      total `shouldBe` 22
