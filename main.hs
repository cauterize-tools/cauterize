module Main where

import Cauterize.Options
import Cauterize.Schema
import Cauterize.Specification

import Text.PrettyPrint.Class

main :: IO ()
main = runWithOptions $ \opts -> parseFile (inputFile opts) >>= render
  where
    render result = case result of
                      (Left e) -> print e
                      (Right r) -> case checkSchema r of
                                      [] -> print $ pretty r -- print $ fromSchema r
                                      es -> print es
