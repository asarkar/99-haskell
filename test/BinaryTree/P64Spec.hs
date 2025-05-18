module BinaryTree.P64Spec (spec) where

import BinaryTree.BinaryTree (Tree (..), fromList)
import BinaryTree.P64
import Test.Hspec

tree64 :: Tree Char
tree64 =
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
      "h",
      "null",
      "null",
      "null",
      "s",
      "null",
      "null",
      "g",
      "null",
      "q",
      "null",
      "e"
    ]

spec :: Spec
spec = do
  describe "layout" $ do
    it "annotates each node according to inorder position and depth" $ do
      -- (traceShow (toList (:[]) t2) t2)
      layout tree64
        `shouldBe` Branch
          ('n', (8, 1))
          ( Branch
              ('k', (6, 2))
              ( Branch
                  ('c', (2, 3))
                  (Branch ('a', (1, 4)) Empty Empty)
                  ( Branch
                      ('h', (5, 4))
                      ( Branch
                          ('g', (4, 5))
                          (Branch ('e', (3, 6)) Empty Empty)
                          Empty
                      )
                      Empty
                  )
              )
              (Branch ('m', (7, 3)) Empty Empty)
          )
          ( Branch
              ('u', (12, 2))
              ( Branch
                  ('p', (9, 3))
                  Empty
                  ( Branch
                      ('s', (11, 4))
                      (Branch ('q', (10, 5)) Empty Empty)
                      Empty
                  )
              )
              Empty
          )
