module Graph.P86Spec where

import Graph.P86
import Test.Hspec

spec :: Spec
spec = do
  describe "kColor" $ do
    it "paint the nodes of a graph in such that adjacent nodes have different colors" $ do
      let v1 = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
      let e1 =
            [ ('a', 'b'),
              ('a', 'e'),
              ('a', 'f'),
              ('b', 'c'),
              ('b', 'g'),
              ('c', 'd'),
              ('c', 'h'),
              ('d', 'e'),
              ('d', 'i'),
              ('e', 'j'),
              ('f', 'h'),
              ('f', 'i'),
              ('g', 'i'),
              ('g', 'j'),
              ('h', 'j')
            ]
      kColor v1 e1
        `shouldMatchList` [ ('a', 1),
                            ('b', 2),
                            ('c', 1),
                            ('d', 2),
                            ('e', 3),
                            ('f', 2),
                            ('g', 1),
                            ('h', 3),
                            ('i', 3),
                            ('j', 2)
                          ]

      let v2 = ['A' .. 'H']
      let e2 =
            [ ('A', 'B'),
              ('A', 'H'),
              ('B', 'C'),
              ('B', 'D'),
              ('C', 'E'),
              ('D', 'E'),
              ('D', 'H'),
              ('E', 'F'),
              ('E', 'H'),
              ('F', 'G'),
              ('F', 'H')
            ]
      kColor v2 e2
        `shouldMatchList` [ ('A', 3),
                            ('B', 1),
                            ('C', 2),
                            ('D', 3),
                            ('E', 1),
                            ('F', 3),
                            ('G', 1),
                            ('H', 2)
                          ]
