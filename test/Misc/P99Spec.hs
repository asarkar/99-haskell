module Misc.P99Spec (spec) where

import Misc.P99
import Test.Hspec

spec :: Spec
spec = do
  describe "crossword" $ do
    it "solves a Crossword puzzle" $ do
      let board =
            [ ".. ..",
              ".. ..",
              "     ",
              ".. . ",
              ".. . ",
              ".... "
            ]

      crossword ["ALPHA", "ARES", "POPPY"] board
        `shouldBe` [ "..P..",
                     "..O..",
                     "ALPHA",
                     "..P.R",
                     "..Y.E",
                     "....S"
                   ]
