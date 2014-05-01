module Main where

import Cauterize.Options
import Cauterize.Schema
import Cauterize.Specification

import Data.List

main :: IO ()
main = runWithOptions $ \opts -> parseFile (inputFile opts) >>= render
  where
    render result = case result of
                      (Left e) -> print e
                      (Right r) -> print $ doThings r -- fromSchema r

doThings s = let m = schemaTypeMap s
             in case m `references` "astruct" of
                  Nothing -> []
                  Just x -> x
