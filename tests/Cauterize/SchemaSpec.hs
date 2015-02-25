module Cauterize.SchemaSpec
  ( spec
  ) where

import Test.Hspec
import Cauterize.Schema

spec :: Spec
spec = do
  describe "parseFile" $ do
    it "can parse a schema from a file" $ do
      s <- parseFile "tests/test_schema.txt"
      case s of
        Left e -> expectationFailure (show e)
        Right _ -> return ()

  describe "parseSchema" $ do
    it "can parse a simple schema" $
      let s = unlines [ "(schema simple_schema 0.0.1"
                      , "  (synonym a_u8 u8))"
                      ]
      in case parseString "" s of
          Left e -> expectationFailure (show e)
          Right _ -> return ()
