module List.P19Spec (spec) where

import List.P19
import Test.Hspec

spec :: Spec
spec = do
  describe "rotate" $ do
    it "rotate a list N places to the left" $ do
      rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] 3
        `shouldBe` "defghabc"
      rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] (-2)
        `shouldBe` "ghabcdef"
