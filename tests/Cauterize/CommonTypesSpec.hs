module Cauterize.CommonTypesSpec
       ( spec
       ) where

import Cauterize.CommonTypes
import Test.Hspec
import Data.Maybe

spec :: Spec
spec = do
  describe "mkIdentifier" $ do
    it "catches bad identifiers" $ do
      mkIdentifier "John" `shouldSatisfy` isNothing
