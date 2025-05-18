module List.P28Spec (spec) where

import List.P28
import Test.Hspec

spec :: Spec
spec = do
  describe "lsort" $ do
    it "sorts a list of lists according to length of sublists" $ do
      lsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
        `shouldBe` ["o", "de", "de", "mn", "abc", "fgh", "ijkl"]

  describe "lfsort" $ do
    it "sorts a list of lists according to their length frequency" $ do
      lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
        `shouldBe` ["ijkl", "o", "abc", "fgh", "de", "de", "mn"]
