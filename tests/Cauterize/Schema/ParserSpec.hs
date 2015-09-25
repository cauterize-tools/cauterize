{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Schema.ParserSpec
       ( spec
       ) where

import Cauterize.CommonTypes
import Cauterize.Schema.Types
import Cauterize.Schema.Parser
import Test.Hspec

import Data.Either

spec :: Spec
spec = do
  describe "parseSchema" $ do
    it "parses a synonym" $ do
      let s = parseSchema "(type foo synonym u8)"
      s `shouldSatisfy` isRight
      s `shouldSatisfy` hasSynNamedFoo
    it "parses a formatted schema" $ do
      let r = do
            s <- parseSchema "(type foo synonym u8)"
            let f = formatSchema s
            s' <- parseSchema f
            return (s == s')
      r `shouldBe` (Right True)
  where
    hasSynNamedFoo
      (Right
       (Schema {
            schemaTypes = [Type {
                               typeName = n,
                               typeDesc = Synonym _
                               }]
            })) = unIdentifier n == "foo"
    hasSynNamedFoo _ = False
