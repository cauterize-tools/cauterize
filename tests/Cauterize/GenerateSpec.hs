module Cauterize.GenerateSpec
  ( spec
  ) where

import Test.Hspec
import Cauterize.Generate

import Cauterize.Common.Types

import qualified Cauterize.Schema as Schema
import qualified Cauterize.Specification as Spec

spec :: Spec
spec = do
  describe "schema generation" $ do
    it "honors size restrictions" $ do
      s <- generateSchemaWith 100 1000 0.9 [ PVSynonym
                                           , PVArray
                                           , PVVector
                                           , PVRecord
                                           , PVCombination
                                           , PVUnion
                                           ]
      let s' = Spec.fromSchema s
      let numBuiltIns = fromEnum (maxBound :: BuiltIn) + 1

      (length . Schema.schemaTypes) s - numBuiltIns `shouldSatisfy` (<= 100)
      (Spec.maxSize . Spec.specSize) s' `shouldSatisfy` (<= 1000)
