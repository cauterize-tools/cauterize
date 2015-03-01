{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Test.Crucible.Options
  ( CrucibleOpts(..)
  , crucibleOptions
  , defaultSchemaCount, defaultInstanceCount
  ) where

import Cauterize.Generate
import Control.Monad (liftM)
import Options.Applicative
import qualified Data.Text.Lazy as T

data CrucibleOpts = CrucibleOpts
  { buildCmds :: [T.Text]
  , runCmd :: T.Text
  , schemaCount :: Maybe Integer
  , instanceCount :: Maybe Integer
  , schemaTypeCount :: Integer -- number of types
  , schemaEncSize :: Integer  -- maximum encoded size
  , allowedPrototypes :: [PrototypeVariant]
  } deriving (Show)

defaultSchemaCount, defaultInstanceCount :: Integer
defaultSchemaCount = 1
defaultInstanceCount = 100

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
  <*> parseSchemaTypeCount
  <*> parseSchemaEncSize
  <*> parseAllowedPrototypes
  where
    parseBuild = many $ option txt ( long "build-cmd"
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
    parseSchemaTypeCount = option auto ( long "type-count"
                                      <> metavar "SCMSIZE"
                                      <> value defaultMaximumTypes
                                      <> help schemaSizeHelp )
    parseSchemaEncSize = option auto ( long "enc-size"
                                    <> metavar "ENCSIZE"
                                    <> value defaultMaximumSize
                                    <> help schemaSizeHelp )
    parseAllowedPrototypes = option parseAlloProtOpt
                                   (long "prototypes"
                                 <> metavar "PROTOTYPES"
                                 <> value defaultAllowedPrototypes
                                 <> help allowedPrototypesHelp )

    buildCmdHelp = "The command to build the generated test client. Can be specified more than once."
    runCmdHelp = "The command to run the built test client. Can be specified more than once."
    schemaCountHelp = "The number of schemas to test."
    instanceCountHelp = "The number of instances of each schema to test."
    schemaSizeHelp = "The number of types to generate in each schema."
    allowedPrototypesHelp = concat [ "Which prototypes to include in schema generation. "
                                   , "Define using a comma-separated list including only the following elements: "
                                   , "array,combination,record,synonym,union,vector."
                                   ]

    txt = liftM T.pack str
    parseAlloProtOpt = do txts <- liftM (T.splitOn ",") txt
                          mapM parseProtoVar txts

-- TODO: parseProtoVar is likely duplicated in Cauterize.Test.Generate as well.
-- See what it would take to deduplicate.
parseProtoVar :: Monad m => T.Text -> m PrototypeVariant
parseProtoVar "synonym" = return PVSynonym
parseProtoVar "array" = return PVArray
parseProtoVar "vector" = return PVVector
parseProtoVar "record" = return PVRecord
parseProtoVar "combination" = return PVCombination
parseProtoVar "union" = return PVUnion
parseProtoVar s = fail (T.unpack s)
