module Main
  ( main
  ) where

import TestSupport

import Data.Maybe
import System.Exit

import qualified Suites.ParseSchema
import qualified Suites.Dynamic

main :: IO ()
main = do
  suiteResults <- mapM testSuite [ Suites.Dynamic.suite
                                 , Suites.ParseSchema.suite
                                 ]
  case sum $ catMaybes suiteResults of
    0 -> exitSuccess
    n -> exitWith (ExitFailure n)
