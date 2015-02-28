module Cauterize.GenerateSpec
  ( spec
  ) where

import Test.Hspec
import Cauterize.Generate

import Text.PrettyPrint
import Text.PrettyPrint.Class

import qualified Cauterize.Specification as Spec

spec :: Spec
spec = do
  describe "schema generation" $ do
    it "honors size restrictions" $ do
      s <- generateSchemaWith 10 1000 0.9 [ PVSynonym
                                          , PVArray
                                          , PVVector
                                          , PVRecord
                                          , PVCombination
                                          , PVUnion
                                          ]
      let s' = Spec.fromSchema s
      print $ Spec.specSize s'
      print $ pretty s'
