{-# OPTIONS -Wno-missing-export-lists #-}

module SpecHook where

import Test.Hspec

hook :: Spec -> Spec
hook = parallel
