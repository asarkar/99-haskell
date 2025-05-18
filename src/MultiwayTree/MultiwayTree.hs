{-# LANGUAGE DerivingStrategies #-}

module MultiwayTree.MultiwayTree where

data Tree a = Node a [Tree a]
  deriving stock (Eq, Show)
