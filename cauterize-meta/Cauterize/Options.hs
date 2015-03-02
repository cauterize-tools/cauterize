module Cauterize.Options
  ( MetaOpts(..)
  , runWithOptions
  ) where

import Options.Applicative

data MetaOpts = MetaOpts
  { specFile :: FilePath
  } deriving (Show)

runWithOptions :: (MetaOpts -> IO ()) -> IO ()
runWithOptions fn = execParser options >>= fn

options :: ParserInfo MetaOpts
options = info (optParser <**> helper)
            ( fullDesc
           <> progDesc "Turn specification files into agnostic interface \
                       \files."
            )

optParser :: Parser MetaOpts
optParser = MetaOpts
  <$> strOption
    ( long "spec"
   <> metavar "FILE_PATH"
   <> help "Input Cauterize specification file."
    )
