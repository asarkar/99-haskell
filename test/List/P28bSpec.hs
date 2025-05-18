module List.P28bSpec (spec) where

import List.P28b
import Test.Hspec

spec :: Spec
spec = do
  describe "lfsort" $ do
    it "sorts a list of lists according to their length frequency" $ do
      lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
        `shouldBe` ["ijkl", "o", "abc", "fgh", "de", "de", "mn"]
