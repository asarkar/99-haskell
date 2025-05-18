module Logic.LogicSpec (spec) where

import Logic.Logic
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "boolean function" $ do
    prop "and' iff both true" $ \a b ->
      and' a b `shouldBe` a && b

    prop "or' iff either true" $ \a b ->
      or' a b `shouldBe` a || b

    prop "nand' is not and'" $ \a b ->
      nand' a b `shouldBe` not (a && b)

    prop "nor' is not or'" $ \a b ->
      nor' a b `shouldBe` not (a || b)

    prop "xor' iff only one true" $ \a b ->
      xor' a b `shouldBe` (a /= b)

    prop "impl' implies consequent is true if antecedent is true" $ \a ->
      impl' True a `shouldBe` a

    prop "impl' does not care if antecedent is false" $ \a ->
      impl' False a `shouldBe` True

    prop "equ' iff the same" $ \a b ->
      equ' a b `shouldBe` a == b
