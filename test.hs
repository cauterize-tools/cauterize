module Main where

import Cauterize.Schema.Arbitrary
import Cauterize.Schema.Types
import Cauterize.Specification.Types

import qualified Cauterize.Specification.Parser as SP

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Text.PrettyPrint.Class

import Control.Monad

main :: IO ()
main = parsePrettyParseIsId

parsePrettyParseIsId :: IO ()
parsePrettyParseIsId = do
  s <- liftM (fromSchema . unValidSchema) $ generate (arbitrary :: Gen ValidSchema)
  let s' = pretty s

  case SP.parseString "" $ show s' of
    (Right s'') -> print $ s == s''
    (Left e) -> print e >> print s'

printArbSpec :: IO ()
printArbSpec = do
  s <- generate (arbitrary :: Gen ValidSchema)
  let s' = unValidSchema s
  case checkSchema s' of
    [] -> print . pretty $ fromSchema s'
    es -> print es
