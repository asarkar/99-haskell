module List.P16Spec (spec) where

import List.P16
import Test.Hspec

spec :: Spec
spec = do
  describe "dropEvery" $ do
    it "drops every N'th element" $ do
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"
