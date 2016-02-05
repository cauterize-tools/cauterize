{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Specification.ParserSpec
       ( spec
       ) where

import Cauterize.Specification.Types
import Cauterize.Specification.Parser
import Cauterize.CommonTypes
import Test.Hspec

import Data.Either

spec :: Spec
spec = do
    describe "parseSpecification" $ do
      it "parses a specification" $ do
        let s = parseSpecification synFoo
        s `shouldSatisfy` isRight
        s `shouldSatisfy` hasSynNamedFoo
      it "parses a formatted specification" $ do
        let r = do
              s <- parseSpecification synFoo
              let f = formatSpecification s
              s' <- parseSpecification f
              return (s == s')
        r `shouldBe` (Right True)
  where
    synFoo = "(type foo synonym (fingerprint 0cb7bd78634eba6f3633dbf0a5f69537aa1916df) (size 1 1) (depth 1) u8)"
    hasSynNamedFoo
      (Right
       (Specification {
            specTypes = [Type {
                               typeName = n,
                               typeDesc = Synonym _,
                               typeDepth = 1
                               }]
            })) = unIdentifier n == "foo"
    hasSynNamedFoo _ = False
