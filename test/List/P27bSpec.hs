module List.P27bSpec (spec) where

import List.P27b
import Test.Hspec

spec :: Spec
spec = do
  describe "group" $ do
    it "returns a list of groups" $ do
      let people =
            [ "aldo",
              "beat",
              "carla",
              "david",
              "evi",
              "flip",
              "gary",
              "hugo",
              "ida"
            ]

      let groups = group [2, 3, 4] people
      length groups `shouldBe` 1260

      let groups' = group [2, 2, 5] people
      length groups' `shouldBe` 756
