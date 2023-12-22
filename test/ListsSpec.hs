module ListsSpec (spec) where

import qualified Data.List as L
import GenLists
import Lists
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "myLast" $ do
    prop "returns the last element" $ do
      forAll (genNEList genNum) $
        \xs -> myLast xs `shouldBe` L.last xs

  describe "myButLast" $ do
    prop "returns the last-but-one element" $ do
      forAll (genNEList genNum `suchThat` ((> 1) . length)) $
        \xs -> myButLast xs `shouldBe` (L.last . L.init) xs

  describe "elementAt" $ do
    prop "returns the K'th element" $ do
      forAll (genNEListAndLen genNum) $
        \(xs, k) -> elementAt xs k `shouldBe` xs !! (k - 1)

  describe "myLength" $ do
    prop "finds the number of elements" $ do
      forAll (genNEList genNum) $
        \xs -> myLength xs `shouldBe` length xs

  describe "myReverse" $ do
    prop "reverses a list" $ do
      forAll (genNEList genNum) $
        \xs -> myReverse xs `shouldBe` reverse xs

  describe "isPalindrome" $ do
    it "finds out whether a list is a palindrome" $ do
      isPalindrome [1 :: Int, 2, 3] `shouldBe` False
      isPalindrome "madamimadam" `shouldBe` True
      isPalindrome [1 :: Int, 2, 4, 8, 16, 8, 4, 2, 1] `shouldBe` True

  describe "flatten" $ do
    it "flattens a nested list structure" $ do
      flatten (Elem (5 :: Int)) `shouldBe` [5]
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem (5 :: Int)]])
        `shouldBe` [1 .. 5]
      flatten (List []) `shouldBe` ([] :: [Int])

  describe "compress" $ do
    it "eliminates consecutive duplicates" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"

  describe "pack" $ do
    prop "packs consecutive duplicates of list elements into sublists" $ do
      forAll (genRepeatedElem genNum) $
        \xs -> pack xs `shouldBe` L.group xs

  describe "encode" $ do
    it "creates Run-length encoding" $ do
      encode "aaaabccaadeeee"
        `shouldBe` [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')]
