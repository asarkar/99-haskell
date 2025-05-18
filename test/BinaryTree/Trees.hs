module BinaryTree.Trees where

import BinaryTree.BinaryTree (Tree (..), fromList)

{- ORMOLU_DISABLE -}
tree4 :: Tree Int
tree4 = fromList read ["1", "2", "2", "4"]
