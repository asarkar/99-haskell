module Misc2Spec (spec) where

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
      isIdentifier "" `shouldBe` False
      isIdentifier "a" `shouldBe` True
      isIdentifier "a1" `shouldBe` True
      isIdentifier "this_is_a_long_identifier" `shouldBe` True
      isIdentifier "This_ends_in_an_underscore_" `shouldBe` False
      isIdentifier "This__has__two__consecutive__underscores" `shouldBe` False
      isIdentifier "1234" `shouldBe` False
      isIdentifier "_legal_in_many_other_languages" `shouldBe` False
      isIdentifier "Fibonacci_sequence_is_1_1_2_3_5_8_13_21_ad_infinitum" `shouldBe` True

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
            [ ['.', '.', ' ', '.', '.'],
              ['.', '.', ' ', '.', '.'],
              [' ', ' ', ' ', ' ', ' '],
              ['.', '.', ' ', '.', ' '],
              ['.', '.', ' ', '.', ' '],
              ['.', '.', '.', '.', ' ']
            ]

      crossword ["ALPHA", "ARES", "POPPY"] board
        `shouldBe` [ ['.', '.', 'P', '.', '.'],
                     ['.', '.', 'O', '.', '.'],
                     ['A', 'L', 'P', 'H', 'A'],
                     ['.', '.', 'P', '.', 'R'],
                     ['.', '.', 'Y', '.', 'E'],
                     ['.', '.', '.', '.', 'S']
                   ]
