module Suites.Dynamic
  ( suite
  ) where

import Control.Exception
import Control.Monad
import TestSupport
import qualified Cauterize.Dynamic as Dynamic
import qualified Cauterize.Dynamic.Meta as Dynamic.Meta
import qualified Data.ByteString as B

suite :: [IO Int]
suite =
  [ testCase "can encode and decode dynamically" testDynamicEncodeDecodeIsEqual
  , testCase "can detect name/type mismatches" testCatchesNameAndTypeMismatch
  ]

-- This test runs itself many times
testDynamicEncodeDecodeIsEqual :: IO ()
testDynamicEncodeDecodeIsEqual = withTestSSM $ \_ spec meta -> replicateM_ 1000 $ do
  t <- Dynamic.Meta.dynamicMetaGen spec meta
  let p = Dynamic.Meta.dynamicMetaPack spec meta t
  let u = Dynamic.Meta.dynamicMetaUnpack spec meta p
  case u of
    Left e -> tf $ "could not unpack dynamically generated type: '" ++ e ++ "'. Type was: " ++ show t ++ ". Bytes were: " ++ show (B.unpack p) ++ "."
    Right t' -> if t' /= t
                  then tf $ "unpacking packed type did not yield equal value: " ++ show t ++ " /= " ++ show t'
                  else tp

testCatchesNameAndTypeMismatch :: IO ()
testCatchesNameAndTypeMismatch = withTestSS $ \_ spec -> (go spec `catch` eh)
  where
    eh (Dynamic.TypeMisMatch _) = tp
    eh e = tf $ "unexpected exception: " ++ show e

    go spec = do
      b <- return $ Dynamic.dynamicPack spec t
      b `seq` tf $ "didn't raise an exception and resulted in the following bytestring: " ++ (show . B.unpack) b

    t = Dynamic.CautType { Dynamic.ctName = "a_u8"
                         , Dynamic.ctDetails = Dynamic.CDBuiltIn (Dynamic.BDu8 1)
                         }

testCatchesContainerMismatch :: IO ()
testCatchesContainerMismatch = withTestSS $ \_ spec -> (go spec `catch` eh)
  where
    eh (Dynamic.TypeMisMatch _) = tp
    eh e = tf $ "unexpected exception: " ++ show e

    go spec = do
      b <- return $ Dynamic.dynamicPack spec t
      b `seq` tf $ "didn't raise an exception and resulted in the following bytestring: " ++ (show . B.unpack) b

    t = Dynamic.CautType { Dynamic.ctName = "a_u8"
                         , Dynamic.ctDetails = Dynamic.CDBuiltIn (Dynamic.BDu16 1)
                         }
