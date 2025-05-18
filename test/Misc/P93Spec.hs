module Misc.P93Spec (spec) where

import Misc.P93
import Test.Hspec

spec :: Spec
spec = do
  describe "arithmetic puzzle" $ do
    it "generates equations" $ do
      let actual = puzzle [2, 3, 5, 7, 11]
      let expected =
            [ "2 = 3 - (5 + 7 - 11)",
              "2 = 3 - 5 - (7 - 11)",
              "2 = 3 - (5 + 7) + 11",
              "2 = 3 - 5 - 7 + 11",
              "2 = (3 * 5 + 7) / 11",
              "2 * (3 - 5) = 7 - 11",
              "2 - (3 - (5 + 7)) = 11",
              "2 - (3 - 5 - 7) = 11",
              "2 - (3 - 5) + 7 = 11",
              "2 - 3 + 5 + 7 = 11"
            ]
      actual `shouldMatchList` expected
