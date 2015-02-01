module Main (main) where

import Options.Applicative

import Cauterize.Test.Generate
import Cauterize.Test.Generate.Options

data Options = Options Command deriving (Show)
data Command = GenerateCom GenerateOpts
  deriving (Show)

genOptions :: Parser Command
genOptions = (GenerateCom . GenerateOpts)
  <$> option auto
    ( long "count" <> metavar "COUNT" <> help countHelp )
  where
    countHelp = "The number of types to generate."

{-
 - TODO:
 -    - allowed prototypes
 -    - maximum length
 -    - minimum length
 -}

optParser :: Parser Options
optParser = Options
  <$> subparser
      ( command "generate"
        ( info genOptions
          ( progDesc "Generate a random schema." ) ) )

options :: ParserInfo Options
options = info (optParser <**> helper)
               (fullDesc <> progDesc "Test infrastructure for Cauterize")

main :: IO ()
main = do
  (Options c) <- execParser options
  case c of
    GenerateCom gos -> printArbSpec gos
