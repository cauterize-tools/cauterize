{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Test.Crucible
  ( runCrucible
  ) where

import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Cauterize.Test.Crucible.Options
import System.Directory
import Control.Monad

data Context = Context
  { specificationPath :: T.Text
  , metaPath :: T.Text
  , currentDir :: T.Text
  } deriving (Show)

runCrucible :: CrucibleOpts -> IO ()
runCrucible opts = inTmpDir $ do
  ctx <- liftM3 Context (return "specPath.spec")
                        (return "metaPath.spec")
                        (liftM T.pack getCurrentDirectory)

  print $ opts { genCmd = expandCmd ctx . genCmd $ opts
               , buildCmd = expandCmd ctx . buildCmd $ opts
               , runCmd = expandCmd ctx . runCmd $ opts
               }

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
  let td = "cauterize-crucible-" ++ show t
  createDirectory td
  setCurrentDirectory td
  a' <- a
  setCurrentDirectory cd
  return a'
