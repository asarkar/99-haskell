module Misc.P96Spec (spec) where

import qualified Control.Monad as M
import Misc.P96
import Test.Hspec

spec :: Spec
spec = do
  describe "isIdentifier" $ do
    it "checks whether a given string is a legal identifier" $ do
      let xs =
            [ ("", False),
              ("a", True),
              ("a1", True),
              ("this_is_a_long_identifier", True),
              ("This_ends_in_an_underscore_", False),
              ("This__has__two__consecutive__underscores", False),
              ("1234", False),
              ("_legal_in_many_other_languages", False),
              ("Fibonacci_sequence_is_1_1_2_3_5_8_13_21_ad_infinitum", True)
            ]
      M.forM_ xs $ \(s, valid) ->
        isIdentifier s `shouldBe` valid
