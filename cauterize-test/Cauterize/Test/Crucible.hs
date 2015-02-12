{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Test.Crucible
  ( runCrucible
  ) where

import Cauterize.Schema.Arbitrary
import Cauterize.Test.Crucible.Options
import Control.Monad
import Data.Time.Clock.POSIX
import System.Directory
import System.Process
import System.Exit
import Test.QuickCheck.Gen
import qualified Cauterize.Schema as SC
import qualified Cauterize.Specification as SP
import qualified Cauterize.Meta as ME
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.PrettyPrint.Class

data Context = Context
  { specificationPath :: T.Text
  , metaPath :: T.Text
  , currentDir :: T.Text
  } deriving (Show)

data RunOutput = RunOutput
  { runCmdStr :: T.Text
  , runStdOut :: T.Text
  , runStdErr :: T.Text
  , runExitCode :: ExitCode
  } deriving (Show)

runWasSuccessful :: RunOutput -> Bool
runWasSuccessful RunOutput { runExitCode = e } = e == ExitSuccess

runCrucible :: CrucibleOpts -> IO ()
runCrucible opts = inTmpDir $ do
  schema <- aSchema (schemaSize opts)
  let spec = SP.fromSchema schema
  let meta = ME.metaFromSpec spec

  T.writeFile "schema.txt" $ T.pack . show . pretty $ schema
  T.writeFile "specification.txt" $ T.pack . show . pretty $ spec
  T.writeFile "meta.txt" $ T.pack . show . ME.prettyMeta $ meta

  ctx <- liftM3 Context (return "specification.txt")
                        (return "meta.txt")
                        (liftM T.pack getCurrentDirectory)

  let opts' = opts { genCmd = expandCmd ctx . genCmd $ opts
                   , buildCmd = expandCmd ctx . buildCmd $ opts
                   , runCmd = expandCmd ctx . runCmd $ opts
                   }

  outputs <- dependentCommands [ genCmd opts', buildCmd opts', runCmd opts' ]
  mapM_ print outputs

shellCmd :: T.Text -> IO RunOutput
shellCmd cmd = do
  (_, Just stdoh, Just stdeh, ph) <- createProcess shelled
  e <- waitForProcess ph
  stdo <- T.hGetContents stdoh
  stde <- T.hGetContents stdeh

  return RunOutput { runCmdStr = cmd
                   , runStdOut = stdo
                   , runStdErr = stde
                   , runExitCode = e
                   }
  where
    shelled = (shell $ T.unpack cmd) { std_out = CreatePipe
                                     , std_err = CreatePipe
                                     }

dependentCommands :: [T.Text] -> IO [RunOutput]
dependentCommands [] = return []
dependentCommands (c:cmds) = do
  c' <- shellCmd c
  if runWasSuccessful c'
    then liftM (c':) (dependentCommands cmds)
    else return $ c':[]

expandCmd :: Context -> T.Text -> T.Text
expandCmd ctx cmd = repSpecPath . repMetaPath . repDirPath $ cmd
  where
    repSpecPath = T.replace "%s" (specificationPath ctx)
    repMetaPath = T.replace "%m" (metaPath ctx)
    repDirPath = T.replace "%d" (currentDir ctx)

inTmpDir :: IO a -> IO a
inTmpDir a = do
  cd <- getCurrentDirectory
  t <- round `fmap` getPOSIXTime :: IO Integer
  let td = "crucible-" ++ show t
  createDirectory td
  setCurrentDirectory td
  a' <- a
  setCurrentDirectory cd
  return a'

aSchema :: Int -> IO SC.Schema
aSchema c = generate $ arbSchemaParam allProtoParams c
  where
    allProtoParams = S.fromList [ ParamSynonym, ParamArray
                                , ParamVector, ParamRecord
                                , ParamCombination , ParamUnion ]
