module Cauterize.Test.Crucible.Options
  ( CrucibleOpts(..)
  , crucibleOptions
  ) where

import Options.Applicative
import qualified Data.Text as T

data CrucibleOpts = CrucibleOpts
  { genCmd :: T.Text
  , buildCmd :: T.Text
  , runCmd :: T.Text
  , schemaCount :: Int
  , instanceCount :: Int
  , schemaSize :: Int
  } deriving (Show)

-- Generate a number of schemas, insert their schemas and meta files into a
-- target generator, run the generator in the test loop, report the exit codes
-- from server and client as results.
--
-- Expanded tokens:
--  %s - the path to the specification file.
--  %m - the path to the meta file.
--  %d - the path to the working directory of the crucible command.
--
-- cauterize-test crucible --generate-cmd="foo --schema=%s --meta=%m --output=%d/cmd"
--                         --build-cmd="cd %d/cmd && cabal build"
--                         --run-cmd="%d/dist/build/cmd/cmd"
--                         --schema-count=5
--                         --instance-count=100
--                         --schema-size=50

crucibleOptions :: Parser CrucibleOpts
crucibleOptions = CrucibleOpts
  <$> option (str >>= toText)
    ( long "generate-cmd" <> metavar "GENCMD" <> help genCmdHelp )
  <*> option (str >>= toText)
    ( long "build-cmd" <> metavar "BLDCMD" <> help buildCmdHelp )
  <*> option (str >>= toText)
    ( long "run-cmd" <> metavar "RUNCMD" <> help runCmdHelp )
  <*> option auto
    ( long "schema-count" <> metavar "SCMCNT" <> help schemaCountHelp )
  <*> option auto
    ( long "instance-count" <> metavar "INSCNT" <> help instanceCountHelp )
  <*> option auto
    ( long "schema-size" <> metavar "SCMSIZE" <> help schemaSizeHelp )
  where
    genCmdHelp = "The command to convert from a specification and meta file into an encoding test client."
    buildCmdHelp = "The command to build the generated test client."
    runCmdHelp = "The command to run the built test client."
    schemaCountHelp = "The number of schemas to test."
    instanceCountHelp = "The number of instances of each schema to test."
    schemaSizeHelp = "The number of types to generate in each schema."

    toText :: String -> ReadM T.Text
    toText = return . T.pack
