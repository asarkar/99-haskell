module BinaryTree.P67A (stringToTree, treeToString) where

import BinaryTree.BinaryTree (Tree (..))
import Control.Applicative ((<|>))
import qualified DList as D
import Parser (Parser (..))
import qualified Parser as P

{-
Problem 67A: (**) A string representation of binary trees.
Write a predicate which generates this string representation.
Then write a predicate which does this inverse; i.e. given the
string representation, construct the tree in the usual form.
Finally, combine the two predicates in a single predicate
which can be used in both directions.
-}

type TreeParser = Parser (Tree Char)

branch :: TreeParser
branch = do
  x <- P.letter
  left <- P.open *> binaryTree <* P.comma
  right <- binaryTree <* P.close
  return $ Branch x left right

emptyTree :: TreeParser
emptyTree = pure Empty

singleton :: TreeParser
singleton = do
  x <- P.letter
  return $ Branch x Empty Empty

binaryTree :: TreeParser
binaryTree = branch <|> singleton <|> emptyTree

stringToTree :: String -> Tree Char
stringToTree = fst . head . P.parse binaryTree

treeToString :: Tree Char -> String
treeToString = D.toList . go D.empty
  where
    go acc Empty = acc
    go acc (Branch x Empty Empty) = acc D.++ D.singleton x
    go acc (Branch x l r) =
      acc
        D.++ D.singleton x
        D.++ D.singleton '('
        D.++ go D.empty l
        D.++ D.singleton ','
        D.++ go D.empty r
        D.++ D.singleton ')'
