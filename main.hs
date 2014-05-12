module Main where

import Cauterize.Options
import Cauterize.Schema.Parser
import Cauterize.Common.Primitives
import Cauterize.Schema.Types
import Cauterize.Specification

import qualified Data.Map as M

main :: IO ()
main = runWithOptions $ \opts -> parseFile (inputFile opts) >>= render
  where
    render (Left s) = print s
    render (Right s) = 
      case checkSchema s of
        [] -> do
            putStrLn $ sigLines $ schemaSigMap s
            print s
            print $ fromSchema s
        es -> print es

sigLines :: M.Map Name Signature -> String
sigLines m = let sigs = map snd $ M.toList m
             in unlines sigs
