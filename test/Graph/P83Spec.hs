{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Graph.P83Spec where

import qualified Control.Monad as M
import qualified Data.List as L
import qualified Data.List.Split as LS
import Graph.P83
import Test.Hspec

spec :: Spec
spec = do
  describe "spanningTrees" $ do
    it "constructs all spanning trees" $ do
      let edges =
            [ ('a', 'b'),
              ('a', 'd'),
              ('b', 'c'),
              ('b', 'e'),
              ('c', 'e'),
              ('d', 'e'),
              ('d', 'f'),
              ('d', 'g'),
              ('e', 'h'),
              ('f', 'g'),
              ('g', 'h')
            ]
      let n = (length . L.nub) $ concatMap (\(u, v) -> [u, v]) edges
      let st = spanningTrees edges
      length st `shouldBe` 28

      M.forM_ st $ \t -> do
        length t `shouldBe` n
        M.forM_ (LS.divvy 2 1 t) $ \e ->
          e `shouldSatisfy` (\[x, y] -> (x, y) `elem` edges || (y, x) `elem` edges)
