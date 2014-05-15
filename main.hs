module Main where

import Cauterize.Options
import Cauterize.Common.Primitives

import Cauterize.Schema as Sc
import Cauterize.Specification as Sp

import qualified Data.Map as M

main :: IO ()
main = runWithOptions $ \opts -> parseFile (inputFile opts) >>= render
  where
    render (Left s) = print s
    render (Right s) = 
      case checkSchema s of
        [] -> putStrLn $ Sp.prettyPrint $ fromSchema s
        es -> print es

sigLines :: M.Map Name Signature -> String
sigLines m = let sigs = map snd $ M.toList m
             in unlines sigs
