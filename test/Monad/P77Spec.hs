module Monad.P77Spec (spec) where

import Monad.P77
import Test.Hspec

spec :: Spec
spec = do
  describe "randomWalkPaths" $ do
    it "returns all 1D random walk paths with n steps" $ do
      randomWalkPaths 0 `shouldBe` [[0]]
      randomWalkPaths 2
        `shouldMatchList` [ [0, -1, -2],
                            [0, -1, -1],
                            [0, -1, 0],
                            [0, 0, -1],
                            [0, 0, 0],
                            [0, 0, 1],
                            [0, 1, 0],
                            [0, 1, 1],
                            [0, 1, 2]
                          ]
