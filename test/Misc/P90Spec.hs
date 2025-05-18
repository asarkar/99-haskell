module Misc.P90Spec (spec) where

import Misc.P90
import Test.Hspec

spec :: Spec
spec = do
  describe "queens" $ do
    it "places n queens so that no two queens are attacking each other" $ do
      let positions = queens 8
      length positions `shouldBe` 92
      [1, 5, 8, 6, 3, 7, 2, 4] `shouldSatisfy` flip elem positions
