module Arithmetic.P41Spec (spec) where

import Arithmetic.P41
import Test.Hspec

spec :: Spec
spec = do
  describe "goldbachList" $ do
    it "finds Goldbach composition of all erven numbers within a given range" $ do
      goldbachList 9 20 `shouldMatchList` [(3, 7), (5, 7), (3, 11), (3, 13), (5, 13), (3, 17)]
