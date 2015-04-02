{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module TestSupport
  ( SchemaFailure(..)
  , testSchema

  , itWithSpec
  ) where

import Test.Hspec

import Control.Exception
import Data.Data
import qualified Cauterize.Schema as Schema
import qualified Cauterize.Specification as Spec
import qualified Data.Text.Lazy as T

itWithSpec :: String -> (Spec.Spec -> IO ()) -> SpecWith ()
itWithSpec s a = it s $ withTestSS $ \_ specification -> a specification

-- Data type to encompass failures.
data SchemaFailure = SchemaFailure { tfMessage :: T.Text
                                   , tfOtherInfo :: [T.Text]
                                   }
  deriving (Show, Data, Typeable)
instance Exception SchemaFailure

testSchema :: FilePath
testSchema = "tests/test_schema.txt"

-- Throw a test failure.
tf :: T.Text -> c
tf m = throw $ SchemaFailure m []

withTestS :: (Schema.Schema -> IO ()) -> IO ()
withTestS f = do
  schemaParse <- Schema.parseFile testSchema
  case schemaParse of
    Left err -> tf $ "Failed to parse test schema: " `T.append` (T.pack . show) err
    Right s -> case Schema.checkSchema s of
                [] -> f s
                es -> tf $ "Failed because of schema errors: " `T.append` (T.pack . show) es

withTestSS :: (Schema.Schema -> Spec.Spec -> IO ()) -> IO ()
withTestSS f = withTestS go
  where
    go schema = let spec = Spec.fromSchema schema
                in f schema spec `catch` reThrow ("Schema was:\n" `T.append` Spec.prettyPrint spec)

reThrow :: T.Text -> SchemaFailure -> c
reThrow msg (SchemaFailure m mm) = throw (SchemaFailure m (msg:mm))
