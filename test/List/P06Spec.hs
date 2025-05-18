module List.P06Spec (spec) where

import List.P06
import Test.Hspec

spec :: Spec
spec = do
  describe "isPalindrome" $ do
    it "finds out whether a list is a palindrome" $ do
      isPalindrome [1 :: Int, 2, 3] `shouldBe` False
      isPalindrome "madamimadam" `shouldBe` True
      isPalindrome [1 :: Int, 2, 4, 8, 16, 8, 4, 2, 1] `shouldBe` True
