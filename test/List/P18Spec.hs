module List.P18Spec (spec) where

import List.P18
import Test.Hspec

spec :: Spec
spec = do
  describe "slice" $ do
    it "extracts a slice" $ do
      slice ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'k'] 3 7
        `shouldBe` "cdefg"
