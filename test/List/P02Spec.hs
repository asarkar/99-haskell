module List.P02Spec (spec) where

import qualified Data.List as L
import List.GenList
import List.P02
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "myButLast" $ do
    prop "returns the last-but-one element" $ do
      forAll (genNEList genNum `suchThat` ((> 1) . length)) $
        \xs -> myButLast xs `shouldBe` (L.last . L.init) xs
