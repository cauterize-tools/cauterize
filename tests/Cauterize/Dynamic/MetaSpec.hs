module Cauterize.Dynamic.MetaSpec
  ( spec
  ) where

import Cauterize.Dynamic.Meta

import Test.Hspec
import TestSupport

import Control.Monad
import qualified Data.ByteString as B

spec :: Spec
spec = do
  describe "dynamic transcoding against test_schema.txt" $ do
    itWithSpecAndMeta "random transcodes are equal" $ \specification meta -> replicateM_ 10000 $ do
      t <- dynamicMetaGen specification meta
      let p = dynamicMetaPack specification meta t
      let u = dynamicMetaUnpack specification meta p
      case u of
        Left e -> expectationFailure $ "could not unpack dynamically generated type: '" ++ e ++ "'. Type was: " ++ show t ++ ". Bytes were: " ++ show (B.unpack p) ++ "."
        Right t' -> if t' /= t
                      then expectationFailure $ "unpacking packed type did not yield equal value: " ++ show t ++ " /= " ++ show t'
                      else return ()
