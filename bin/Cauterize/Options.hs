module Cauterize.Options where

import           Cauterize.Version   (versionString)
import           Options.Applicative

data CautOpts = CautOpts
  { schemaFile :: FilePath
  , specPath   :: FilePath
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
  `mappend` progDesc "Compile a Cauterize schema into a Cauterize specification"
   )
  where
  o = flag' Nothing (long "version" `mappend` hidden)
   <|> (Just <$> optParser)

optParser :: Parser CautOpts
optParser = CautOpts
  <$> argument str
    ( metavar "SCHEMA"
   `mappend` help "Cauterize schema input file."
    )
  <*> argument str
    ( metavar "SPEC"
   `mappend` help "Cauterize specification output file."
    )
