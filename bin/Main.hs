module Main (main) where

import Cauterize.Options

import Cauterize.Schema as Sc
import Cauterize.Specification as Sp

import System.Exit

import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = runWithOptions $ \opts -> Sc.parseFile (schemaFile opts) >>= render (specPath opts) >>= exitWith
  where
    render _ (Left s) = print s >> return (ExitFailure (-1))
    render outFile (Right s) =
      case checkSchema s of
        [] -> T.writeFile outFile (Sp.prettyPrint . fromSchema $ s) >> return ExitSuccess
        es -> print es >> return (ExitFailure (length es))
