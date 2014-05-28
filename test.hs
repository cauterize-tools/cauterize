module Main where

import Cauterize.Schema.Arbitrary
import Cauterize.Schema.Types
import Cauterize.Specification.Types
import Cauterize.Specification.Parser

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Text.PrettyPrint.Class

main :: IO ()
main = parsePrettyParseIsId

parsePrettyParseIsId :: IO ()
parsePrettyParseIsId = do
  (Right s) <- parseFile "rando_spec.scm"
  let s' = pretty s
  let (Right s'') = parseString "" $ show s'
  print $ s == s''

printArbSpec :: IO ()
printArbSpec = do
  s <- generate (arbitrary :: Gen ValidSchema)
  let s' = unValidSchema s
  case checkSchema s' of
    [] -> print . pretty $ fromSchema s'
    es -> print es
