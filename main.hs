module Main where

import Cauterize.Options
import Cauterize.Schema
import Cauterize.Specification

main :: IO ()
main = runWithOptions $ \opts -> parseFile (inputFile opts) >>= render
  where
    render result = case result of
                      (Left e) -> print e
                      (Right r) -> print $ fromSchema r
