{-# LANGUAGE DeriveDataTypeable #-}
module Main
  ( main
  ) where

import Control.Exception
import Control.Monad
import Data.Data
import System.Exit
import qualified Cauterize.Dynamic.Meta as Dynamic
import qualified Cauterize.Meta as Meta
import qualified Cauterize.Schema as Schema
import qualified Cauterize.Specification as Spec
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

-- Data type to encompass failures.
data TestFailure = TestFailure { tfMessage :: String
                               , tfOtherInfo :: [String]
                               }
  deriving (Show, Data, Typeable)
instance Exception TestFailure

-- Throw a test failure.
tf :: String -> c
tf m = throw $ TestFailure m []

tp :: IO ()
tp = return ()

-- Run a test case.
testCase :: String -> IO () -> IO Int
testCase n a = runCase `catch` handler
  where
    runCase :: IO Int
    runCase = a >> return 0

    handler :: TestFailure -> IO Int
    handler (TestFailure { tfMessage = m, tfOtherInfo = oi }) = do
      putStrLn $ "FAILURE: " ++ n
      putStrLn m
      case oi of
        [] -> return 1
        _ -> do putStrLn "OTHER INFO:"
                mapM_ putStrLn oi
                return 1

testSchema :: FilePath
testSchema = "tests/test_schema.txt"

withTestS :: (Schema.Schema -> IO ()) -> IO ()
withTestS f = do
  schemaParse <- Schema.parseFile testSchema
  case schemaParse of
    Left err -> tf $ "Failed to parse test schema: " ++ show err
    Right s -> case Schema.checkSchema s of
                [] -> f s
                es -> tf $ "Failed because of schema errors: " ++ show es

withTestSS :: (Schema.Schema -> Spec.Spec -> IO ()) -> IO ()
withTestSS f = withTestS go
  where
    go schema = let spec = Spec.fromSchema schema
                in f schema spec `catch` reThrow ("Schema was:\n" ++ Spec.prettyPrint spec)

withTestSSM :: (Schema.Schema -> Spec.Spec -> Meta.Meta -> IO ()) -> IO ()
withTestSSM f = withTestSS go
  where
    go schema spec = let meta = Meta.metaFromSpec spec
                     in f schema spec meta `catch` reThrow ("Meta was:\n" ++ show (Meta.prettyMeta meta))

reThrow :: String -> TestFailure -> c
reThrow msg (TestFailure m mm) = throw (TestFailure m (msg:mm))

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
