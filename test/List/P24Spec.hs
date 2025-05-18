module List.P24Spec (spec) where

import qualified Data.Set as S
import List.P24
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  -- TODO: Can't get it to throw exception, some lazy voodoo!
  -- it "throws an error when given a negative number of elements to select" $ do
  --   B.evaluate (rndSelect [1 :: Int] (-1)) `shouldThrow` anyErrorCall

  describe "diffSelect" $ do
    prop "draws N different random numbers from a set 1..M" $ do
      forAll ((<$> chooseInt (0, 100)) . (,) =<< chooseInt (1, 100)) $
        \(n, m) -> do
          xs <- diffSelect n m
          length (S.fromList xs) `shouldBe` min n m
