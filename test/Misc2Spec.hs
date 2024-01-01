module Misc2Spec (spec) where

import qualified Control.Monad as M
import Misc2
import Test.Hspec

spec :: Spec
spec = do
  describe "fullWords" $ do
    it "converts integers to full words" $ do
      fullWords 175 `shouldBe` "one-seven-five"
      fullWords 0 `shouldBe` "zero"
      fullWords 1 `shouldBe` "one"

  describe "isIdentifier" $ do
    it "checks whether a given string is a legal identifier" $ do
      let xs =
            [ ("", False),
              ("a", True),
              ("a1", True),
              ("this_is_a_long_identifier", True),
              ("This_ends_in_an_underscore_", False),
              ("This__has__two__consecutive__underscores", False),
              ("1234", False),
              ("_legal_in_many_other_languages", False),
              ("Fibonacci_sequence_is_1_1_2_3_5_8_13_21_ad_infinitum", True)
            ]
      M.forM_ xs $ \(s, valid) ->
        isIdentifier s `shouldBe` valid

  describe "sudoku" $ do
    it "solves a Sudoku puzzle" $ do
      let board =
            [ [0, 0, 4, 8, 0, 0, 0, 1, 7],
              [6, 7, 0, 9, 0, 0, 0, 0, 0],
              [5, 0, 8, 0, 3, 0, 0, 2, 4],
              [3, 0, 0, 7, 4, 0, 1, 0, 0],
              [0, 6, 9, 0, 0, 0, 7, 8, 0],
              [0, 0, 1, 0, 6, 9, 0, 0, 5],
              [1, 0, 0, 0, 8, 0, 3, 0, 0],
              [0, 0, 0, 0, 0, 6, 0, 9, 1],
              [2, 4, 0, 0, 0, 1, 5, 0, 0]
            ]
      sudoku board
        `shouldBe` [ [9, 3, 4, 8, 2, 5, 6, 1, 7],
                     [6, 7, 2, 9, 1, 4, 8, 5, 3],
                     [5, 1, 8, 6, 3, 7, 9, 2, 4],
                     [3, 2, 5, 7, 4, 8, 1, 6, 9],
                     [4, 6, 9, 1, 5, 3, 7, 8, 2],
                     [7, 8, 1, 2, 6, 9, 4, 3, 5],
                     [1, 9, 7, 5, 8, 2, 3, 4, 6],
                     [8, 5, 3, 4, 7, 6, 2, 9, 1],
                     [2, 4, 6, 3, 9, 1, 5, 7, 8]
                   ]

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
