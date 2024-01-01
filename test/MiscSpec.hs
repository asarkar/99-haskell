module MiscSpec (spec) where

import qualified Data.List as L
import Misc
import Test.Hspec

spec :: Spec
spec = do
  describe "queens" $ do
    it "places n queens so that no two queens are attacking each other" $ do
      let positions = queens 8
      length positions `shouldBe` 92
      [1, 5, 8, 6, 3, 7, 2, 4] `shouldSatisfy` flip elem positions

  describe "knightsTour" $ do
    it "visits every square exactly once" $ do
      let tour = knightsTour 8 (const (63 ==)) (1, 1)
      length tour `shouldBe` 64
      tour `shouldMatchList` [(x, y) | x <- [1 .. 8], y <- [1 .. 8]]

  describe "knightsTo" $ do
    it "ends at the given position" $ do
      let tour = knightsTo 8 (1, 1)
      length tour `shouldBe` 64
      tour `shouldMatchList` [(x, y) | x <- [1 .. 8], y <- [1 .. 8]]
      L.last tour `shouldBe` (1, 1)

  describe "closedKnights" $ do
    it "ends at a position from where the start position is reachable" $ do
      let tour = closedKnights 8
      length tour `shouldBe` 64
      tour `shouldMatchList` [(x, y) | x <- [1 .. 8], y <- [1 .. 8]]
      L.last tour `shouldSatisfy` (`elem` [(2, 3), (3, 2)])
      head tour `shouldBe` (1, 1)
