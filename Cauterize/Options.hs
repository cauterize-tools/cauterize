module Cauterize.Options where

import Options.Applicative

data CautOpts = CautOpts
  { inputFile :: String
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
    ( long "input"
   <> metavar "FILE_PATH"
   <> help "Input Cauterize schema file."
    )
