module Cauterize.Options where

import Options.Applicative

data CautOpts = CautOpts
  { targetLanguage :: String
  , inputFile :: String
  , outputDirectory :: String
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
    ( long "target-lang"
   <> metavar "TARGET_LANGAUGE"
   <> help "Target language for which to generate code."
    )
  <*> strOption
    ( long "input"
   <> metavar "FILE_PATH"
   <> help "Input Cauterize schema file."
    )
  <*> strOption
    ( long "output"
   <> metavar "DIRECTORY_PATH"
   <> help "Output Cauterize directory."
    )
