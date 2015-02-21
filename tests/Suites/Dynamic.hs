module Suites.Dynamic
  ( suite
  ) where

import Control.Monad
import TestSupport
import qualified Cauterize.Dynamic.Meta as Dynamic
import qualified Data.ByteString as B

suite :: [IO Int]
suite =
  [ testCase "can encode and decode dynamically" testDynamicEncodeDecodeIsEqual
  ]

-- This test runs itself many times
testDynamicEncodeDecodeIsEqual :: IO ()
testDynamicEncodeDecodeIsEqual = withTestSSM $ \_ spec meta -> replicateM_ 1000 $ do
  t <- Dynamic.dynamicMetaGen spec meta
  let p = Dynamic.dynamicMetaPack spec meta t
  let u = Dynamic.dynamicMetaUnpack spec meta p
  case u of
    Left e -> tf $ "could not unpack dynamically generated type: '" ++ e ++ "'. Type was: " ++ show t ++ ". Bytes were: " ++ show (B.unpack p) ++ "."
    Right t' -> if t' /= t
                  then tf $ "unpacking packed type did not yield equal value: " ++ show t ++ " /= " ++ show t'
                  else tp
