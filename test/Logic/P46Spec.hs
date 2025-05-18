module Logic.P46Spec (spec) where

import Logic.Logic
import Logic.P46
import Test.Hspec

spec :: Spec
spec = do
  describe "table" $ do
    it "generates the truth table of a given logical expression in two variables" $ do
      table (\a b -> and' a (or' a b))
        `shouldMatchList` [ (True, True, True),
                            (True, False, True),
                            (False, True, False),
                            (False, False, False)
                          ]
