{-# LANGUAGE DeriveDataTypeable #-}
module TestSupport
  ( SchemaFailure(..)
  , testSchema

  , itWithSpec
  , itWithSpecAndMeta
  ) where

import Test.Hspec

import Control.Exception
import Data.Data
import qualified Cauterize.Meta as Meta
import qualified Cauterize.Schema as Schema
import qualified Cauterize.Specification as Spec

itWithSpec :: String -> (Spec.Spec -> IO ()) -> SpecWith ()
itWithSpec s a = it s $ withTestSS $ \_ specification -> a specification

itWithSpecAndMeta :: String -> (Spec.Spec -> Meta.Meta -> IO ()) -> SpecWith ()
itWithSpecAndMeta s a = it s $ withTestSSM $ \_ specification meta -> a specification meta

-- Data type to encompass failures.
data SchemaFailure = SchemaFailure { tfMessage :: String
                                   , tfOtherInfo :: [String]
                                   }
  deriving (Show, Data, Typeable)
instance Exception SchemaFailure

testSchema :: FilePath
testSchema = "tests/test_schema.txt"

-- Throw a test failure.
tf :: String -> c
tf m = throw $ SchemaFailure m []

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

reThrow :: String -> SchemaFailure -> c
reThrow msg (SchemaFailure m mm) = throw (SchemaFailure m (msg:mm))
