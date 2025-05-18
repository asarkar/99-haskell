module Misc.P92Spec (spec) where

import qualified Control.Monad as M
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import Misc.P92
import Test.Hspec

spec :: Spec
spec = do
  describe "von koch's conjecture" $ do
    it "generates graceful labeling of a tree" $ do
      let xs =
            [ [(1, 7), (7, 2), (7, 3), (3, 5), (3, 6), (5, 4)],
              [ (1, 6),
                (2, 6),
                (3, 6),
                (4, 6),
                (5, 6),
                (5, 7),
                (5, 8),
                (8, 9),
                (5, 10),
                (10, 11),
                (11, 12),
                (11, 13),
                (13, 14)
              ]
            ]

      M.forM_ xs $ \edges -> do
        let labeling = vonKochLabeling edges
        let vertices = (L.nub . concat) [[u, v] | (u, v) <- edges]
        let labels = Set.fromList [v | (_, v) <- Map.assocs labeling]
        let n = length vertices

        Set.fromList (Map.keys labeling) `shouldBe` Set.fromList vertices
        length labels `shouldBe` n
        M.forM_ labels $ \l -> do
          l `shouldSatisfy` (\x -> x >= 1 && x <= n)
