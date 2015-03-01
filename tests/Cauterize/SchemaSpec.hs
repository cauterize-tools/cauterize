{-# LANGUAGE OverloadedStrings #-}
module Cauterize.SchemaSpec
  ( spec
  ) where

import Test.Hspec
import Cauterize.Schema

import qualified Data.Text.Lazy as T

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
      let s = T.unlines [ "(schema simple_schema 0.0.1"
                        , "  (synonym a_u8 u8))"
                        ]
      in case parseText "" s of
          Left e -> expectationFailure (show e)
          Right _ -> return ()
