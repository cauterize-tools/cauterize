module Cauterize.Test.Test.Options
  ( TestOptions(..)
  , testOptions
  ) where

import Options.Applicative

data TestOptions = TestOptions
  { specName :: FilePath
  , metaName :: FilePath
  } deriving (Show)

testOptions :: Parser TestOptions
testOptions = TestOptions
  <$> option str
    ( long "spec-path" <> metavar "SPECPATH" <> help specPathHelp)
  <*> option str
    ( long "meta-path" <> metavar "METAPATH" <> help metaPathHelp)
  where
    specPathHelp = "Path to the input specification file."
    metaPathHelp = "Path to the input meta file."
