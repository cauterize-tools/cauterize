module Main (main) where

import Cauterize.Options

import Cauterize.Schema as Sc
import Cauterize.Schema.Checker as Sc
import Cauterize.Specification as Sp

import System.Exit

import qualified Data.Text.IO as T

main :: IO ()
main = runWithOptions $ \opts -> Sc.parseSchemaFromFile (schemaFile opts) >>= render (specPath opts) >>= exitWith
  where
    render _ (Left s) = print s >> return (ExitFailure (-1))
    render outFile (Right s) =
      case Sc.checkSchema s of
        Right cs -> T.writeFile outFile (Sp.formatSpecification . Sp.mkSpecification $ cs) >> return ExitSuccess
        Left es -> print es >> return (ExitFailure 1)
