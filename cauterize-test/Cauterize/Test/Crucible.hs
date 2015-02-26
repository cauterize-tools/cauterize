{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Test.Crucible
  ( runCrucible
  ) where

import Cauterize.Schema.Arbitrary
import Cauterize.Test.Crucible.Options
import Control.Concurrent
import Control.Monad
import Data.Time.Clock.POSIX
import Data.Maybe
import System.Directory
import System.Exit
import System.Process
import System.IO
import Test.QuickCheck.Gen
import Text.PrettyPrint.Class
import qualified Data.ByteString as B
import qualified Cauterize.Meta as ME
import qualified Cauterize.Schema as SC
import qualified Cauterize.Specification as SP
import qualified Cauterize.Dynamic.Meta as D
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

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

data TestResult = TestPass
                | TestError { testErrorMessage :: String }
                | TestFail { testFailEncoded :: B.ByteString  }
  deriving (Show)

runWasSuccessful :: RunOutput -> Bool
runWasSuccessful RunOutput { runExitCode = e } = e == ExitSuccess

runCrucible :: CrucibleOpts -> IO ()
runCrucible opts = do
  t <- round `fmap` getPOSIXTime :: IO Integer
  inNewDir ("crucible-" ++ show t) $
    forM_ ([0..runCount-1] :: [Int]) $
      \ix -> inNewDir ("run-" ++ show ix) go
  where
    runCount = fromMaybe defaultSchemaCount (schemaCount opts)
    instCount = fromMaybe 1 (instanceCount opts)
    go = do
          -- Generate a schema. From this, also compile a specification file and a meta
          -- file. Write them to disk.
          schema <- aSchema $ fromMaybe defaultSchemaSize (schemaSize opts)
          let spec = SP.fromSchema schema
          let meta = ME.metaFromSpec spec

          T.writeFile "schema.txt" $ T.pack . show . pretty $ schema
          T.writeFile "specification.txt" $ T.pack . show . pretty $ spec
          T.writeFile "meta.txt" $ T.pack . show . ME.prettyMeta $ meta

          -- Construct a context with the paths to the specification and meta files
          -- along with the current directory.
          ctx <- liftM3 Context (return "specification.txt")
                                (return "meta.txt")
                                (liftM T.pack getCurrentDirectory)

          -- Chain together the commands specified on the command line after expanding
          -- their variables. Print summaries of the commands.
          let buildCmds' = map (expandCmd ctx) (buildCmds opts)
          let runCmd' = expandCmd ctx (runCmd opts)

          buildOutputs <- runDependentCommands buildCmds'

          if all runWasSuccessful buildOutputs
            then runCrucibleForSchemaInstance runCmd' spec meta instCount >>= renderResults opts
            else putStrLn "Build failure:" >> printResult (last buildOutputs)

renderResults :: CrucibleOpts -> [TestResult] -> IO ()
renderResults opts rs = go rs 0
  where
    denomStr = "(" ++ show (schemaCount opts) ++ "*" ++ show (instanceCount opts) ++ ")"
    successStr = "Success! 0/" ++ denomStr ++ " instances had problems."
    failStr n = "FAIL FAIL FAIL! " ++ show n ++ "/" ++ denomStr ++ " instances had problems."

    go [] 0 = putStrLn successStr >> exitSuccess
    go [] n = putStrLn (failStr n) >> exitWith (ExitFailure n)
    go (t:rest) failures = case t of
                              TestPass -> go rest failures
                              TestError m -> printErr m >> go rest (failures + 1)
                              TestFail b -> printFail b >> go rest (failures + 1)

    printErr e = putStrLn $ "Error: " ++ e
    printFail b = putStrLn $ "Failure for encoded bytes: " ++ (show . B.unpack) b

runCrucibleForSchemaInstance :: T.Text -> SP.Spec -> ME.Meta -> Int -> IO [TestResult]
runCrucibleForSchemaInstance cmd spec meta count = replicateM count $ do
  -- Create the process. Grab its stdin and stdout.
  (Just stdih, Just stdoh, Just _, ph) <- createProcess shelled
  result <- runTest stdih stdoh spec meta
  _ <- waitForProcess ph
  return result
  where
    shelled = (shell $ T.unpack cmd) { std_out = CreatePipe
                                     , std_err = CreatePipe
                                     , std_in = CreatePipe
                                     }


runTest :: Handle -> Handle -> SP.Spec -> ME.Meta -> IO TestResult
runTest ih oh spec meta = do
  -- Generate a type according to the specification, then pack it to binary.
  mt <- D.dynamicMetaGen spec meta
  let packed = D.dynamicMetaPack spec meta mt

  -- Setup a thread to do the writing.
  encodeDone <- newEmptyMVar
  _ <- forkFinally (encoder packed encodeDone) (\_ -> putMVar encodeDone ())

  -- Begin decoding from the process' stdout.
  hdr <- liftM (D.dynamicMetaUnpackHeader meta) (B.hGet oh hlen)
  result <- case hdr of
              -- Unable to unpack the header.
              Left err -> return $ TestError err

              -- Got a header.
              Right (mh, _) -> do
                -- Read the payload bytes.
                payload <- B.hGet oh (fromIntegral . D.metaLength $ mh)

                -- Try to unpack the types from the payload bytes.
                return $ case D.dynamicMetaUnpackFromHeader spec meta mh payload of
                          Left str -> TestError { testErrorMessage = str }
                          Right (mt', rest ) | not (B.null rest) -> TestError { testErrorMessage =
                                                                      "Not all bytes were consumed: " ++ (show . B.unpack) rest }
                                             | otherwise -> if mt /= mt'
                                                              then TestFail { testFailEncoded = packed }
                                                              else TestPass
  _ <- takeMVar encodeDone
  return result
  where
    hlen = fromIntegral $ ME.metaTypeLength meta + ME.metaDataLength meta
    encoder packed done = do
      B.hPut ih packed
      hClose ih
      putMVar done ()

runShellCmd :: T.Text -> IO RunOutput
runShellCmd cmd = do
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

runDependentCommands :: [T.Text] -> IO [RunOutput]
runDependentCommands [] = return []
runDependentCommands (c:cmds) = do
  c' <- runShellCmd c
  if runWasSuccessful c'
    then liftM (c':) (runDependentCommands cmds)
    else return [c']

expandCmd :: Context -> T.Text -> T.Text
expandCmd ctx cmd = repSpecPath . repMetaPath . repDirPath $ cmd
  where
    repSpecPath = T.replace "%s" (specificationPath ctx)
    repMetaPath = T.replace "%m" (metaPath ctx)
    repDirPath = T.replace "%d" (currentDir ctx)

inNewDir :: String -> IO a -> IO a
inNewDir name a = do
  cd <- getCurrentDirectory
  createDirectory name
  setCurrentDirectory name
  a' <- a
  setCurrentDirectory cd
  return a'

aSchema :: Int -> IO SC.Schema
aSchema c = generate $ arbSchemaParam allProtoParams c
  where
    allProtoParams = S.fromList [ ParamSynonym, ParamArray
                                , ParamVector, ParamRecord
                                , ParamCombination , ParamUnion ]

printResult :: RunOutput -> IO ()
printResult RunOutput { runCmdStr = cs, runStdOut = so, runStdErr = se, runExitCode = ec } =
  case ec of
    ExitSuccess -> T.putStrLn "SUCCESS"
    ExitFailure c -> do
      T.putStrLn $ "FAILED: " `T.append` (T.pack . show) c
      T.putStrLn $ "## Command String: " `T.append` cs
      T.putStrLn "## Standard output:"
      T.putStr so
      T.putStrLn "## Standard error:"
      T.putStr se
