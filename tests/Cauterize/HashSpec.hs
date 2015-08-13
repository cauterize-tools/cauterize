{-# LANGUAGE OverloadedStrings #-}
module Cauterize.HashSpec
  ( spec
  ) where

import Cauterize.Hash

import Test.Hspec
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "hashToBytes" $ do
    it "creates hashes 20 bytes in length" $ do
      hashToBytes h0 `shouldSatisfy` (\h -> 20 == length h)
      hashToBytes h1 `shouldSatisfy` (\h -> 20 == length h)
  where
    h0 = mkHash "cauterize is neat"
    h1 = mkHash "i've spent so much time on this library..."
       
