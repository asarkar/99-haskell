module List.P11Spec (spec) where

import List.GenList
import List.P11
import List.P12
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "encodeModified" $ do
    prop "creates a run-length encoded list" $ do
      forAll (genRepeatedElem genNum) $
        \xs -> do
          let encoded = encodeModified xs
          let decoded = decodeModified encoded
          decoded `shouldBe` xs
