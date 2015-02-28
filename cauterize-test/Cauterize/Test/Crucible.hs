{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Test.Crucible
  ( runCrucible
  ) where

import Cauterize.Generate
import Cauterize.Test.Crucible.Options
import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.Time.Clock.POSIX
import System.Directory
import System.Exit
import System.IO
import System.Process
import Text.PrettyPrint.Class
import qualified Cauterize.Dynamic.Meta as D
import qualified Cauterize.Meta as ME
import qualified Cauterize.Specification as SP
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Context = Context
  { specificationPath :: T.Text
  , metaPath :: T.Text
  , currentDir :: T.Text
  } deriving (Show)

data BuildCmdOutput = BuildCmdOutput
  { buildCmdStr :: T.Text
  , buildStdOut :: T.Text
  , buildStdErr :: T.Text
  , buildExitCode :: ExitCode
  } deriving (Show)

data TestOutput = TestOutput TestResult String B.ByteString D.MetaType
  deriving (Show)

data TestResult = TestPass
                | TestError { testErrorMessage :: String }
                | TestFail
  deriving (Show)

runCrucible :: CrucibleOpts -> IO ()
runCrucible opts = do
  putStrLn "Running crucible."
  putStrLn $ "  Schema count: " ++ show runCount
  putStrLn $ "  Types per schema: " ++ show typeCount
  putStrLn $ "  Instance from each schema: " ++ show instCount
  putStrLn $ "  Maximum encoded size of each type: " ++ show maxEncSize

  t <- round `fmap` getPOSIXTime :: IO Integer
  failCounts <- inNewDir ("crucible-" ++ show t) $
                  forM [0..runCount-1] $
                    \ix -> inNewDir ("schema-" ++ show ix) go
  case sum failCounts of
    0 -> exitSuccess
    n -> exitWith (ExitFailure n)
  where
    runCount = fromMaybe defaultSchemaCount (schemaCount opts)
    instCount = fromMaybe defaultInstanceCount (instanceCount opts)
    typeCount = fromMaybe defaultSchemaTypeCount (schemaTypeCount opts)
    maxEncSize = fromMaybe defaultSchemaEncSize (schemaEncSize opts)

    -- Creates a schema of the specified size.
    aSchema c s = generateSchemaWith c s 0.95 allProtoParams
    allProtoParams = [ minBound .. maxBound ]

    go = do
          -- Generate a schema. From this, also compile a specification file and a meta
          -- file. Write them to disk.
          schema <- aSchema typeCount maxEncSize
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

          -- Chain together the commands specified on the command line after
          -- expanding their variables. Run the commands in order. If any
          -- command fails in the build sequence, the remaining commands will
          -- not be run.
          let buildCmds' = map (expandCmd ctx) (buildCmds opts)
          let runCmd' = expandCmd ctx (runCmd opts)

          buildOutputs <- runDependentCommands buildCmds'

          -- If all the build commands are successful, go ahead and run several
          -- binary instances conforming to the schema against the generated
          -- code under test.
          --
          -- If anything failed, print a summary of the failure.
          if all runWasSuccessful buildOutputs
            then testCmdWithSchemaInstances runCmd' spec meta instCount >>= renderResults
            else putStrLn "Build failure:" >> printResult (last buildOutputs) >> return 1

-- | Run each command in order. If any command fails, do not run the remaining
-- commands.
runDependentCommands :: [T.Text] -> IO [BuildCmdOutput]
runDependentCommands [] = return []
runDependentCommands (c:cmds) = do
  c' <- runBuildCmd c
  if runWasSuccessful c'
    then liftM (c':) (runDependentCommands cmds)
    else return [c']

-- | Convenience function that evaluates to True when a RunOutput represents a
-- successful termination.
runWasSuccessful :: BuildCmdOutput -> Bool
runWasSuccessful BuildCmdOutput { buildExitCode = e } = e == ExitSuccess

-- | Run the specified text as a build command. Package the outputs.
runBuildCmd :: T.Text -> IO BuildCmdOutput
runBuildCmd cmd = do
  (_, Just stdoh, Just stdeh, ph) <- createProcess shelled
  e <- waitForProcess ph
  stdo <- T.hGetContents stdoh
  stde <- T.hGetContents stdeh

  return BuildCmdOutput { buildCmdStr = cmd
                        , buildStdOut = stdo
                        , buildStdErr = stde
                        , buildExitCode = e
                        }
  where
    shelled = (shell $ T.unpack cmd) { std_out = CreatePipe
                                     , std_err = CreatePipe
                                     }

-- Tests the ability of an executable to transcode an instance of a type from
-- the schema that correstponds to the passed specification and meta file.
--
-- One instance is tested per invocation of the process.
--
-- NOTE: currently there is no timeout. If the child process hangs, everything
-- hangs.
--
-- TODO: come up with timeout mechanism.
testCmdWithSchemaInstances :: T.Text  -- ^ the path to the exectuable to test
                           -> SP.Spec -- ^ the specification from which to generate the instance
                           -> ME.Meta -- ^ the meta file from which to generate the instance
                           -> Integer -- ^ how many instances to test
                           -> IO [TestOutput]
testCmdWithSchemaInstances cmd spec meta count = replicateM (fromIntegral count) $ do
  -- Create the process. Grab its stdin and stdout.
  (Just stdih, Just stdoh, Just stdeh, ph) <- createProcess shelled
  (result, unpacked, packed) <- runTest stdih stdoh spec meta

  -- Wait for the process to terminate, collect the result of stdout, package
  -- everything up properly according to the exit status of the process.
  e <- waitForProcess ph
  errorOutput <- hGetContents stdeh
  r <- case e of
          ExitSuccess -> return result
          ExitFailure c -> return TestError { testErrorMessage = "Client process exited with failure code: " ++ show c }
  return (TestOutput r errorOutput packed unpacked)
  where
    shelled = (shell $ T.unpack cmd) { std_out = CreatePipe
                                     , std_err = CreatePipe
                                     , std_in = CreatePipe
                                     }

-- Do something pretty with a list of TestOutput types.
renderResults :: [TestOutput] -> IO Int
renderResults rs = go rs 0
  where
    successStr = "\nSchema success!"
    failStr n = "\nSCHEMA HAD " ++ show n ++ "FAILURES!"

    go [] 0 = putStrLn successStr >> return 0
    go [] n = putStrLn (failStr n) >> return n
    go (TestOutput t e b mt:rest) failures = case t of
                                              TestPass -> go rest failures
                                              TestError m -> printErr m >> go rest (failures + 1)
                                              TestFail -> printFail >> go rest (failures + 1)
      where
        printErr m = do putStrLn $ "Error: " ++ m
                        putStrLn $ "Error encoded bytes: " ++ (show . B.unpack) b
                        putStrLn $ "Standard error output: " ++ e
                        putStrLn $ "Show metatype: " ++ show mt
        printFail = do putStrLn $ "Failure for encoded bytes: " ++ (show . B.unpack) b
                       putStrLn $ "Standard error output: " ++ e
                       putStrLn $ "Show metatype: " ++ show mt

-- Output the result of a build command.
printResult :: BuildCmdOutput -> IO ()
printResult BuildCmdOutput { buildCmdStr = cs, buildStdOut = so, buildStdErr = se, buildExitCode = ec } =
  case ec of
    ExitSuccess -> T.putStrLn "SUCCESS"
    ExitFailure c -> do
      T.putStrLn $ "FAILED: " `T.append` (T.pack . show) c
      T.putStrLn $ "## Command String: " `T.append` cs
      T.putStrLn "## Standard output:"
      T.putStr so
      T.putStrLn "## Standard error:"
      T.putStr se


-- Run a single test against a binary instance of a schema.
runTest :: Handle -- ^ stdin of the process under test
        -> Handle -- ^ stdout of the process under test
        -> SP.Spec -- ^ specification derived from the schema under test
        -> ME.Meta -- ^ meta from the specification that MUST be the previous arugument (sorry)
        -> IO (TestResult, D.MetaType, B.ByteString)
runTest ih oh spec meta = do
  -- Generate a type according to the specification, then pack it to binary.
  mt <- D.dynamicMetaGen spec meta
  let packed = D.dynamicMetaPack spec meta mt

  -- We keep this here as a sanity check. If this hits, it *IS* an error. We
  -- don't want to continue if this happens. Ever.
  let packedLen = fromIntegral $ B.length packed
  let specMaxLen = SP.maxSize $ SP.specSize spec
  when (packedLen > (specMaxLen + fromIntegral hlen))
       (error $ "LENGTH OF BS TOO LONG! Expected "
             ++ show packedLen
             ++ " to be less than "
             ++ show specMaxLen ++ ".")

  -- Setup a *THREAD* to do write the encoded type to the process-under-test's
  -- stdin. This is in a separate thread because it's likely we can deadlock
  -- for larger types due to OS buffering rules.
  encodeDone <- newEmptyMVar
  _ <- flip forkFinally (\_ -> putMVar encodeDone ()) $ do
      B.hPut ih packed
      hClose ih
      putMVar encodeDone ()

  -- Begin decoding from the process' stdout.
  hdrBytes <- B.hGet oh hlen
  let hdr = D.dynamicMetaUnpackHeader meta hdrBytes

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
                                                              then TestFail
                                                              else TestPass
  -- Make sure the encoding thread is finished.
  _ <- takeMVar encodeDone

  -- Dump something on the console to represent the running state.
  case result of
    TestPass -> putStr "."
    _ -> putStr "X"
  hFlush stdout

  return (result, mt, packed)
  where
    hlen = fromIntegral $ ME.metaTypeLength meta + ME.metaDataLength meta

-- Use a context to expand variables in a command.
expandCmd :: Context -> T.Text -> T.Text
expandCmd ctx cmd = repSpecPath . repMetaPath . repDirPath $ cmd
  where
    repSpecPath = T.replace "%s" (specificationPath ctx)
    repMetaPath = T.replace "%m" (metaPath ctx)
    repDirPath = T.replace "%d" (currentDir ctx)

-- Create a directory, and perform an IO action with that new directory as the
-- working directory of the IO action.
inNewDir :: String -> IO a -> IO a
inNewDir name a = do
  cd <- getCurrentDirectory
  createDirectory name
  setCurrentDirectory name
  a' <- a
  setCurrentDirectory cd
  return a'
