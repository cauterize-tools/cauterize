module Main
  ( main
  ) where

import TestSupport

import Control.Monad
import System.Exit
import qualified Cauterize.Dynamic.Meta as Dynamic
import qualified Data.ByteString as B

main :: IO ()
main = do
  failures <- liftM sum $ sequence tests
  case failures of
    0 -> exitSuccess
    n -> exitWith (ExitFailure n)
  where
    tests = [ testCase "can parse schema" testCanParseTestSchema
            , testCase "can encode and decode dynamically" testDynamicEncodeDecodeIsEqual
            ]

{- ### Test Cases ### -}

-- Ensure we can parse a test.
testCanParseTestSchema :: IO ()
testCanParseTestSchema = withTestS $ const (return ())

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
