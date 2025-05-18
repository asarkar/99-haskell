{-# OPTIONS -Wno-incomplete-uni-patterns #-}

module Logic.P48Spec (spec) where

import Logic.Logic
import Logic.P48
import Test.Hspec

spec :: Spec
spec = do
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
