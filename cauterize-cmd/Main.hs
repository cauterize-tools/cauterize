module Main (main) where

import Cauterize.Options

import Cauterize.Schema as Sc
import Cauterize.Specification as Sp

main :: IO ()
main = runWithOptions $ \opts -> Sc.parseFile (inputFile opts) >>= render
  where
    render (Left s) = print s
    render (Right s) = 
      case checkSchema s of
        [] -> putStrLn $ Sp.prettyPrint $ fromSchema s
        es -> print es
