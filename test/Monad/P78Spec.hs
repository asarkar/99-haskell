module Monad.P78Spec (spec) where

import Monad.P78
import Test.Hspec

spec :: Spec
spec = do
  describe "collatz" $ do
    it "counts the number of steps in the Collatz sequence" $ do
      collatz 1 `shouldBe` 0
      collatz 2 `shouldBe` 1
      collatz 31 `shouldBe` 106
