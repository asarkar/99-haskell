module Logic.P50Spec (spec) where

import Logic.P50
import Test.Hspec

spec :: Spec
spec = do
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
