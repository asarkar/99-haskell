module List.P14Spec (spec) where

import qualified Data.List.Split as LS
import List.GenList
import List.P14
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "dupli" $ do
    prop "duplicates the elements" $ do
      forAll (genList genNum) $
        \xs -> do
          let ys = LS.chunksOf 2 (dupli xs)
          length ys `shouldBe` length xs
          and (zipWith (flip (all . (==))) ys xs) `shouldBe` True
