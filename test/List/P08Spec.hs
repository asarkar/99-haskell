module List.P08Spec (spec) where

import List.P08
import Test.Hspec

spec :: Spec
spec = do
  describe "compress" $ do
    it "eliminates consecutive duplicates" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"
