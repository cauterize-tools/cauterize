module Main (main) where

import Cauterize.Specification as Sp

import Cauterize.Options
import Cauterize.Meta.Types
import Cauterize.Meta.Pretty

import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = runWithOptions $ \opts -> Sp.parseFile (specFile opts) >>= render
  where
    render (Left s) = print s
    render (Right s) = T.putStrLn $ prettyMeta $ metaFromSpec s
