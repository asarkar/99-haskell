module BinaryTree.P65Spec (spec) where

import BinaryTree.BinaryTree (Tree (..), fromList)
import BinaryTree.P65
import Test.Hspec

tree65 :: Tree Char
tree65 =
  fromList
    head
    [ "n",
      "k",
      "u",
      "c",
      "m",
      "p",
      "null",
      "a",
      "e",
      "null",
      "null",
      "null",
      "q",
      "null",
      "null",
      "d",
      "g"
    ]

spec :: Spec
spec = do
  describe "layout2" $ do
    it "annotates each node according to height and depth" $ do
      layout2 tree65
        `shouldBe` Branch
          ('n', (15, 1))
          ( Branch
              ('k', (7, 2))
              ( Branch
                  ('c', (3, 3))
                  (Branch ('a', (1, 4)) Empty Empty)
                  ( Branch
                      ('e', (5, 4))
                      (Branch ('d', (4, 5)) Empty Empty)
                      (Branch ('g', (6, 5)) Empty Empty)
                  )
              )
              (Branch ('m', (11, 3)) Empty Empty)
          )
          ( Branch
              ('u', (23, 2))
              ( Branch
                  ('p', (19, 3))
                  Empty
                  (Branch ('q', (21, 4)) Empty Empty)
              )
              Empty
          )
