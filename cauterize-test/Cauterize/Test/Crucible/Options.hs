module Cauterize.Test.Crucible.Options
  ( CrucibleOpts(..)
  , crucibleOptions
  , defaultSchemaCount, defaultInstanceCount, defaultSchemaSize
  ) where

import Control.Monad (liftM)
import Options.Applicative
import qualified Data.Text as T

data CrucibleOpts = CrucibleOpts
  { buildCmds :: [T.Text]
  , runCmd :: T.Text
  , schemaCount :: Maybe Int
  , instanceCount :: Maybe Int
  , schemaSize :: Maybe Int
  } deriving (Show)

defaultSchemaCount, defaultInstanceCount, defaultSchemaSize :: Int
defaultSchemaCount = 1
defaultInstanceCount = 100
defaultSchemaSize = 10

-- Generate a number of schemas, insert their schemas and meta files into a
-- target generator, run the generator in the test loop, report the exit codes
-- from server and client as results.
--
-- Expanded tokens:
--  %s - the path to the specification file.
--  %m - the path to the meta file.
--  %d - the path to the working directory of the crucible command.
--
-- cauterize-test crucible --build-cmd="foo --schema=%s --meta=%m --output=%d/cmd"
--                         --build-cmd="cd %d/cmd && cabal build"
--                         --run-cmd="%d/dist/build/cmd/cmd"
--                         --schema-count=5
--                         --instance-count=100
--                         --schema-size=50

crucibleOptions :: Parser CrucibleOpts
crucibleOptions = CrucibleOpts
  <$> parseBuild
  <*> parseRun
  <*> parseSchemaCount
  <*> parseInstanceCount
  <*> parseSchemaSize
  where
    parseBuild = some $ option txt ( long "build-cmd"
                                  <> metavar "BLDCMD"
                                  <> help buildCmdHelp )
    parseRun = option txt ( long "run-cmd"
                         <> metavar "RUNCMD"
                         <> help runCmdHelp )
    parseSchemaCount = optional $ option auto ( long "schema-count"
                                             <> metavar "SCMCNT"
                                             <> help schemaCountHelp )
    parseInstanceCount = optional $ option auto ( long "instance-count"
                                               <> metavar "INSCNT"
                                               <> help instanceCountHelp )
    parseSchemaSize = optional $ option auto ( long "schema-size"
                                            <> metavar "SCMSIZE"
                                            <> help schemaSizeHelp )

    buildCmdHelp = "The command to build the generated test client. Can be specified more than once."
    runCmdHelp = "The command to run the built test client. Can be specified more than once."
    schemaCountHelp = "The number of schemas to test."
    instanceCountHelp = "The number of instances of each schema to test."
    schemaSizeHelp = "The number of types to generate in each schema."

    txt = liftM T.pack str
