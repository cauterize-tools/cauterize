module Main where

import Cauterize.Schema.Arbitrary
import Cauterize.Schema.Types
import Cauterize.Specification.Types

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Text.PrettyPrint.Class

main :: IO ()
main = do
  s <- generate (arbitrary :: Gen ValidSchema)
  let s' = unValidSchema s
  case checkSchema s' of
    [] -> print . pretty $ fromSchema s'
    es -> print es
  
