module Lists3Spec (spec) where

import qualified Control.Exception.Base as B
import qualified Data.List as L
import qualified Data.Set as S
import GenList
import Lists3
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "insertAt" $ do
    prop "inserts an element at a given position" $ do
      forAll (genNEListAndLen genNum) $
        \(xs, n) -> insertAt 1 xs n !! (n - 1) `shouldBe` 1

    it "throws an error when the given position is less than 1" $ do
      B.evaluate (insertAt 'X' "abcd" (-1)) `shouldThrow` anyErrorCall

  describe "range" $ do
    it "creates an increasing range" $ do
      range 1 5 `shouldBe` [1, 2, 3, 4, 5]

    it "creates a decreasing range" $ do
      range 5 1 `shouldBe` [5, 4, 3, 2, 1]

    it "creates a singleton range" $ do
      range 1 1 `shouldBe` [1]

  describe "rndSelect" $ do
    prop "extracts a given number of randomly selected elements" $ do
      forAll (genNEListAndLen genNum) $
        \(xs, n) -> do
          ys <- rndSelect xs n
          length ys `shouldBe` n
          all (`elem` xs) ys `shouldBe` True

  -- TODO: Can't get it to throw exception, some lazy voodoo!
  -- it "throws an error when given a negative number of elements to select" $ do
  --   B.evaluate (rndSelect [1 :: Int] (-1)) `shouldThrow` anyErrorCall

  describe "diffSelect" $ do
    prop "draws N different random numbers from a set 1..M" $ do
      forAll ((<$> chooseInt (0, 100)) . (,) =<< chooseInt (1, 100)) $
        \(n, m) -> do
          xs <- diffSelect n m
          length (S.fromList xs) `shouldBe` min n m

  describe "rndPerm" $ do
    prop "generates a random permutation" $ do
      forAll (genList genNum) $
        \xs -> do
          ys <- rndPerm xs

          L.sort ys `shouldBe` L.sort xs

  describe "combinations" $ do
    it "generates all combinations" $ do
      let xs = combinations 3 "abcdef"
      xs
        `shouldBe` [ "abc",
                     "abd",
                     "abe",
                     "abf",
                     "acd",
                     "ace",
                     "acf",
                     "ade",
                     "adf",
                     "aef",
                     "bcd",
                     "bce",
                     "bcf",
                     "bde",
                     "bdf",
                     "bef",
                     "cde",
                     "cdf",
                     "cef",
                     "def"
                   ]

  describe "group" $ do
    it "returns a list of groups" $ do
      let people =
            [ "aldo",
              "beat",
              "carla",
              "david",
              "evi",
              "flip",
              "gary",
              "hugo",
              "ida"
            ]

      let groups = group [2, 3, 4] people
      length groups `shouldBe` 1260

      let groups' = group [2, 2, 5] people
      length groups' `shouldBe` 756

  describe "lsort" $ do
    it "sorts a list of lists according to length of sublists" $ do
      lsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
        `shouldBe` ["o", "de", "de", "mn", "abc", "fgh", "ijkl"]

  describe "lfsort" $ do
    it "sorts a list of lists according to their length frequency" $ do
      lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
        `shouldBe` ["ijkl", "o", "abc", "fgh", "de", "de", "mn"]
