{-# LANGUAGE DeriveDataTypeable #-}
module TestSupport
  ( TestFailure(..)
  , testSchema
  , testSuite
  , tf, tp
  , testCase
  , withTestS
  , withTestSS
  , withTestSSM
  ) where

import Control.Exception
import Control.Monad
import Data.Data
import qualified Cauterize.Meta as Meta
import qualified Cauterize.Schema as Schema
import qualified Cauterize.Specification as Spec

-- Data type to encompass failures.
data TestFailure = TestFailure { tfMessage :: String
                               , tfOtherInfo :: [String]
                               }
  deriving (Show, Data, Typeable)
instance Exception TestFailure

testSchema :: FilePath
testSchema = "tests/test_schema.txt"

-- Throw a test failure.
tf :: String -> c
tf m = throw $ TestFailure m []

-- Test passes
tp :: IO ()
tp = return ()

testSuite :: [IO Int] -> IO (Maybe Int)
testSuite cs = do
  failures <- liftM sum $ sequence cs
  case failures of
    0 -> return Nothing
    n -> return (Just n)

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
