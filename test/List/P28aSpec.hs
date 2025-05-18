module List.P28aSpec (spec) where

import List.P28a
import Test.Hspec

spec :: Spec
spec = do
  describe "lsort" $ do
    it "sorts a list of lists according to length of sublists" $ do
      lsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
        `shouldBe` ["o", "de", "de", "mn", "abc", "fgh", "ijkl"]
