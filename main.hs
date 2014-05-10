module Main where

import Cauterize.Options
import Cauterize.Schema.Parser
import Cauterize.Schema.Types

main :: IO ()
main = runWithOptions $ \opts -> parseFile (inputFile opts) >>= render
  where
    render (Left s) = print s
    render (Right s) = 
      case checkSchema s of
        [] -> do
            print $ schemaSigMap s
            print s
        es -> print es
