module List.P25Spec (spec) where

import qualified Data.List as L
import List.GenList
import List.P25
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "rndPerm" $ do
    prop "generates a random permutation" $ do
      forAll (genList genNum) $
        \xs -> do
          ys <- rndPerm xs

          L.sort ys `shouldBe` L.sort xs
