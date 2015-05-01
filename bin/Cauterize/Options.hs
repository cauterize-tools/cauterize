module Cauterize.Options where

import Options.Applicative

data CautOpts = CautOpts
  { schemaFile :: FilePath
  , specPath :: FilePath
  } deriving (Show)

runWithOptions :: (CautOpts -> IO ()) -> IO ()
runWithOptions fn = execParser options >>= fn

options :: ParserInfo CautOpts
options = info (optParser <**> helper)
            ( fullDesc
           <> progDesc "Process Cauterize schema files."
            )

optParser :: Parser CautOpts
optParser = CautOpts
  <$> strOption
    ( long "schema"
   <> metavar "FILE_PATH"
   <> help "Input Cauterize schema file."
    )
  <*> strOption
    ( long "output"
   <> metavar "OUTPUT_PATH"
   <> help "Output path for specification file."
    )
