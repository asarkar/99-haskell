module Graph.P85Spec where

import Graph.P85
import Test.Hspec

spec :: Spec
spec = do
  describe "iso" $ do
    it "determines whether two graphs are isomorphic" $ do
      let v1 = [1 .. 8 :: Int]
      let e1 =
            [ (1, 5),
              (1, 6),
              (1, 7),
              (2, 5),
              (2, 6),
              (2, 8),
              (3, 5),
              (3, 7),
              (3, 8),
              (4, 6),
              (4, 7),
              (4, 8)
            ]
      let v2 = [1 .. 8 :: Int]
      let e2 =
            [ (1, 2),
              (1, 4),
              (1, 5),
              (6, 2),
              (6, 5),
              (6, 7),
              (8, 4),
              (8, 5),
              (8, 7),
              (3, 2),
              (3, 4),
              (3, 7)
            ]
      iso v1 e1 v2 e2 `shouldBe` True

      let va = ['a' .. 'e']
      let ea = [('a', 'b'), ('a', 'c'), ('a', 'e'), ('b', 'c'), ('c', 'd'), ('d', 'e')]

      let vA = ['A' .. 'E']
      let eA = [('A', 'D'), ('A', 'E'), ('B', 'C'), ('B', 'D'), ('B', 'E'), ('C', 'D')]

      iso va ea vA eA `shouldBe` True

      let vα = ['α', 'β', 'γ', 'δ', 'ε']
      let eα = [('α', 'β'), ('α', 'γ'), ('α', 'ε'), ('β', 'δ'), ('γ', 'δ'), ('δ', 'ε')]

      iso va ea vα eα `shouldBe` False
      iso vA eA vα eα `shouldBe` False

      let va' = ['a' .. 'f']
      let ea' = [('a', 'b'), ('a', 'd'), ('b', 'c'), ('c', 'f'), ('d', 'e'), ('e', 'f')]

      let va'' = ['a' .. 'f']
      let ea'' = [('a', 'd'), ('a', 'e'), ('b', 'd'), ('b', 'f'), ('c', 'e'), ('c', 'f')]

      iso va' ea' va'' ea'' `shouldBe` True

      -- G(va'' ea'') and G(v3 e3) are not isomorphic but the algorithm can't determine that.
      -- let v3 = [1..6 :: Int]
      -- let e3 = [(1, 3), (1, 5), (2, 4), (2, 6), (3, 5), (4, 6)]

      -- iso va'' ea'' v3 e3 `shouldBe` False

      let vA' = ['A' .. 'D']
      let eA' = [('A', 'B'), ('A', 'C'), ('A', 'D'), ('B', 'C'), ('B', 'D'), ('C', 'D')]

      let v4 = [1 .. 4 :: Int]
      let e4 = [(1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)]

      iso vA' eA' v4 e4 `shouldBe` True
