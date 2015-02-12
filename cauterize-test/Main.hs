module Main (main) where

import Options.Applicative

import Cauterize.Test.Generate
import Cauterize.Test.Crucible
import Cauterize.Test.Generate.Options
import Cauterize.Test.Crucible.Options

data Options = Options Command deriving (Show)
data Command = GenerateCom GenerateOpts
             | CrucibleCom CrucibleOpts
  deriving (Show)

optParser :: Parser Options
optParser = Options
  <$> subparser
      ( command "generate"
        ( info (fmap GenerateCom genOptions)
          ( progDesc "Generate a random schema." ) )
     <> command "crucible"
        ( info (fmap CrucibleCom crucibleOptions)
          ( progDesc "Test a generator against many schemas." ) ) )

options :: ParserInfo Options
options = info (optParser <**> helper)
               (fullDesc <> progDesc "Test infrastructure for Cauterize")

main :: IO ()
main = do
  (Options c) <- execParser options
  case c of
    GenerateCom gOpts -> printArbSpec gOpts
    CrucibleCom cOpts -> runCrucible cOpts
