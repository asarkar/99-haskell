{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS -Wno-incomplete-patterns #-}

module BinaryTreeParser (parse, tree) where

import Control.Applicative (Alternative, empty, (<|>))
import qualified Data.Char as C
import Tree (Tree (..))

{-
tree ::= branch | singleton | empty
branch ::= letter open tree comma tree close
singleton ::= letter
empty ::= ""
letter ::= 'a' ... 'z'
open ::= '('
close ::= ')'
comma ::= ','
-}

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

item :: Parser Char
item =
  Parser
    ( \case
        [] -> []
        (x : xs) -> [(x, xs)]
    )

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f pa =
    Parser
      ( \s -> case parse pa s of
          [] -> []
          [(x, s')] -> [(f x, s')]
      )

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\s -> [(x, s)])

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa =
    Parser
      ( \s -> case parse pf s of
          [] -> []
          -- Unwrap the function and fmap over pa.
          [(f, s')] -> parse (fmap f pa) s'
      )

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= f =
    Parser
      ( \s -> case parse pa s of
          [] -> []
          [(x, s')] -> parse (f x) s'
      )

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const [])

  (<|>) :: Parser a -> Parser a -> Parser a
  pa <|> pa' =
    Parser
      ( \s -> case parse pa s of
          [] -> parse pa' s
          xs -> xs
      )

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

letter :: Parser Char
letter = sat C.isAsciiLower

open :: Parser Char
open = sat (== '(')

close :: Parser Char
close = sat (== ')')

comma :: Parser Char
comma = sat (== ',')

type TreeParser = Parser (Tree Char)

branch :: TreeParser
branch = do
  x <- letter
  left <- open *> tree <* comma
  right <- tree <* close
  return $ Branch x left right

emptyTree :: TreeParser
emptyTree = pure Empty

singleton :: TreeParser
singleton = do
  x <- letter
  return $ Branch x Empty Empty

tree :: TreeParser
tree = branch <|> singleton <|> emptyTree
