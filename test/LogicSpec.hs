{-# OPTIONS -Wno-incomplete-uni-patterns #-}

module LogicSpec (spec) where

import Logic
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

  describe "table" $ do
    it "generates the truth table of a given logical expression in two variables" $ do
      table (\a b -> and' a (or' a b))
        `shouldMatchList` [ (True, True, True),
                            (True, False, True),
                            (False, True, False),
                            (False, False, False)
                          ]
  describe "tablen" $ do
    it "generates the truth table of a given logical expression in n variables" $ do
      tablen 3 (\[a, b, c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
        `shouldMatchList` [ [False, False, False, False],
                            [False, False, True, False],
                            [False, True, False, False],
                            [False, True, True, True],
                            [True, False, False, False],
                            [True, False, True, True],
                            [True, True, False, False],
                            [True, True, True, True]
                          ]

    describe "gray" $ do
      it "generates gray codes" $ do
        gray 3 `shouldBe` ["000", "001", "011", "010", "110", "111", "101", "100"]

    describe "huffman" $ do
      it "constructs Huffman encoding for the given characters" $ do
        let freq = [('a', 45), ('b', 13), ('c', 12), ('d', 16), ('e', 9), ('f', 5)]
        huffman freq
          `shouldMatchList` [ ('a', "0"),
                              ('b', "101"),
                              ('c', "100"),
                              ('d', "111"),
                              ('e', "1101"),
                              ('f', "1100")
                            ]
