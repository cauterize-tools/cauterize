module Cauterize.Specification.Parser
  ( parseFile
  , parseString
  ) where

import Text.Parsec
import Text.Parsec.String

import Cauterize.Specification.Types

type ASTSpec = Spec String

parseFile :: FilePath -> IO (Either ParseError ASTSpec)
parseFile path = readFile path >>= parseString path

parseString :: FilePath -> String -> IO (Either ParseError ASTSpec)
parseString path str = 
  return $ case parse parseSpec path str of
              Left e -> Left e
              Right s -> Right s

parseSpec :: Parser ASTSpec
parseSpec = undefined
