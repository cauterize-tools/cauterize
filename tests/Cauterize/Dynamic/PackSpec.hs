{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Dynamic.PackSpec
  ( spec
  ) where

import Cauterize.Dynamic.Pack as P

import qualified Data.ByteString as B
import Cauterize.Specification.Compile
import Cauterize.Schema.Parser
import Cauterize.Dynamic.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "dynamicPack -> dynamicPackRange" $ do
    it "subtracts out the offset" $ do
      let (Right schema) = parseSchema "(type test_range range 181538 13054296747032613673)"
      let sp = mkSpecification schema
      let ty = CautType "test_range" (CDRange 1759352636039645935)
      let expected = [0xcd, 0x85, 0xc7, 0xa9, 0xe6, 0x79, 0x6a, 0x18]
      let bs = B.unpack $ dynamicPack sp ty
      bs `shouldBe` expected
