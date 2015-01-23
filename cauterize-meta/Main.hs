module Main (main) where

import Cauterize.Specification as Sp

import Cauterize.Options
import Cauterize.Meta.Types
import Cauterize.Meta.Pretty

main :: IO ()
main = runWithOptions $ \opts -> Sp.parseFile (specFile opts) >>= render
  where
    render (Left s) = print s
    render (Right s) = print $ prettyMeta $ metaFromSpec s
