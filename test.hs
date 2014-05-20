module Main where

import Cauterize.Schema.Arbitrary

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Text.PrettyPrint.Class

main :: IO ()
main = do
  s <- generate (arbitrary :: Gen ValidSchema)
  print . pretty $ unValidSchema s
