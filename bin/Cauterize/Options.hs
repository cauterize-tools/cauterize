module Cauterize.Options where

import Options.Applicative
import Cauterize.Version (versionString)

data CautOpts = CautOpts
  { schemaFile :: FilePath
  , specPath :: FilePath
  } deriving (Show)

runWithOptions :: (CautOpts -> IO ()) -> IO ()
runWithOptions fn = do
  mopts <- execParser options
  case mopts of
    Just opts -> fn opts
    Nothing -> putStr versionString

options :: ParserInfo (Maybe CautOpts)
options = info (helper <*> o)
   ( fullDesc
  <> progDesc "Compile a Cauterize schema into a Cauterize specification"
   )
  where
  o = flag' Nothing (long "version" <> hidden)
   <|> (Just <$> optParser)

optParser :: Parser CautOpts
optParser = CautOpts
  <$> argument str
    ( metavar "SCHEMA"
   <> help "Cauterize schema input file."
    )
  <*> argument str
    ( metavar "SPEC"
   <> help "Cauterize specification output file."
    )
