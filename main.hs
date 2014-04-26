module Main where

import Cauterize.Options
import Cauterize.Schema.Parser

main :: IO ()
main = runWithOptions $ \opts -> parseFile (inputFile opts) >>= print
