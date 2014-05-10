module Main where

import Cauterize.Options
import Cauterize.Schema.Parser
import Cauterize.Schema.Types

import Text.PrettyPrint.Class

main :: IO ()
main = runWithOptions $ \opts -> parseFile (inputFile opts) >>= render
  where
    render = print
