module Main (main) where

import Cauterize.Options

import Cauterize.Schema as Sc
import Cauterize.Specification as Sp

import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = runWithOptions $ \opts -> Sc.parseFile (schemaFile opts) >>= render
  where
    render (Left s) = print s
    render (Right s) =
      case checkSchema s of
        [] -> T.putStrLn $ Sp.prettyPrint $ fromSchema s
        es -> print es
