module Cauterize.SpecificationSpec
  ( spec
  ) where

import Test.Hspec

import qualified Cauterize.Schema as Schema
import qualified Cauterize.Specification as Spec

spec :: Spec
spec = do
  describe "size calculation" $ do
    it "calculates the correct size of a schema with a single u8 synonym" $ do
      let schemaText = "(schema test_schema 0.0.0 (synonym a_u8 u8))"
      schemaText `asSpec` \Spec.Spec { Spec.specSize = Spec.RangeSize rsMin rsMax } -> do
        rsMin `shouldBe` 1
        rsMax `shouldBe` 1
    it "calculates the correct size of a schema with a single s64 synonym" $ do
      let schemaText = "(schema test_schema 0.0.0 (synonym a_s64 s64))"
      schemaText `asSpec` \Spec.Spec { Spec.specSize = Spec.RangeSize rsMin rsMax } -> do
        rsMin `shouldBe` 8
        rsMax `shouldBe` 8
    it "calculates the correct size of a union" $ do
      let schemaText = unlines [ "(schema test_schema 0.0.0"
                               , "  (union option"
                               , "    (fields"
                               , "      (field nothing)"
                               , "      (field some s64))))"
                               ]
      schemaText `asSpec` \Spec.Spec { Spec.specSize = Spec.RangeSize rsMin rsMax } -> do
        rsMin `shouldBe` 1
        rsMax `shouldBe` 9

asSpec :: String -> (Spec.Spec -> Expectation) -> Expectation
asSpec txt go =
  case Schema.parseString "asSpec" txt of
    Left err -> expectationFailure $ show err
    Right schema -> go $ Spec.fromSchema schema
