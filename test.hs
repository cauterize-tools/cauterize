module Main (main) where

import Cauterize.Schema.Arbitrary
import qualified Cauterize.Schema as SC
import qualified Cauterize.Specification as SP

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Text.PrettyPrint.Class

import Control.Monad

main :: IO ()
main = -- parsePrettyParseIsId
       printArbSpec

parseExample :: IO ()
parseExample = do
  r <- SC.parseFile "examples/atomicObjectBathroomMonitor.scm"

  case r of
    (Left e) -> print e
    (Right v) ->
      case SC.checkSchema v of
        [] -> putStrLn $ SP.prettyPrint $ SP.fromSchema v
        es -> print es

parsePrettyParseIsId :: IO ()
parsePrettyParseIsId = do
  s <- liftM (SP.fromSchema . unValidSchema) $ generate (arbitrary :: Gen ValidSchema)
  let s' = pretty s
  let r = SP.parseString "" $ show s'

  case r of
    (Right s'') -> print $ s == s''
    (Left e) -> print e >> print s'

printArbSpec :: IO ()
printArbSpec = do
  s <- liftM unValidSchema $ generate (arbitrary :: Gen ValidSchema)
  case SC.checkSchema s of
    [] -> print . pretty $ SP.fromSchema s
    es -> print es
