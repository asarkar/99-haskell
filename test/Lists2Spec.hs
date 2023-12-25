module Lists2Spec (spec) where

import qualified Data.List as L
import qualified Data.List.Split as LS
import GenList
import Lists2
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "encodeModified" $ do
    prop "creates a run-length encoded list" $ do
      forAll (genRepeatedElem genNum) $
        \xs -> do
          let encoded = encodeModified xs
          let decoded = decodeModified encoded
          decoded `shouldBe` xs

  describe "dupli" $ do
    prop "duplicates the elements" $ do
      forAll (genList genNum) $
        \xs -> do
          let ys = LS.chunksOf 2 (dupli xs)
          length ys `shouldBe` length xs
          and (zipWith (flip (all . (==))) ys xs) `shouldBe` True

  describe "dropEvery" $ do
    it "drops every N'th element" $ do
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"

  describe "split" $ do
    prop "splits a list into two parts" $ do
      forAll (genNEListAndLen genNum) $
        \(xs, n) -> do
          split xs n `shouldBe` L.splitAt n xs

  describe "slice" $ do
    it "extracts a slice" $ do
      slice ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'k'] 3 7
        `shouldBe` "cdefg"

  describe "rotate" $ do
    it "rotate a list N places to the left" $ do
      rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] 3
        `shouldBe` "defghabc"
      rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] (-2)
        `shouldBe` "ghabcdef"

  describe "removeAt" $ do
    it "removes the K'th element" $ do
      removeAt 2 "abcd" `shouldBe` ('b', "acd")
