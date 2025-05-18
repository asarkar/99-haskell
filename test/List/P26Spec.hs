module List.P26Spec (spec) where

import List.P26
import Test.Hspec

spec :: Spec
spec = do
  describe "combinations" $ do
    it "generates all combinations" $ do
      let xs = combinations 3 "abcdef"
      xs
        `shouldBe` [ "abc",
                     "abd",
                     "abe",
                     "abf",
                     "acd",
                     "ace",
                     "acf",
                     "ade",
                     "adf",
                     "aef",
                     "bcd",
                     "bce",
                     "bcf",
                     "bde",
                     "bdf",
                     "bef",
                     "cde",
                     "cdf",
                     "cef",
                     "def"
                   ]
