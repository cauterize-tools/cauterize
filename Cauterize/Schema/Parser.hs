module Cauterize.Schema.Parser (parseFile) where

import Control.Monad

import Text.Parsec
import Text.Parsec.String

import Cauterize.Schema.Types
import Cauterize.Schema.Utils

parseFile :: FilePath -> IO (Either ParseError Schema)
parseFile path = do
  d <- readFile path
  return $ case parse parseSchema path d of
              Left e -> Left e
              Right s -> Right s

parseSchema :: Parser Schema
parseSchema = parens $ do
  _ <- string "schema"
  sName <- spaces1 >> quoted
  sVersion <- spaces1 >> quoted
  sForms <- option [] $ spaces1 >> parseForm `sepBy` spaces1

  return (Schema sName sVersion sForms)

parseForm :: Parser Form
parseForm = liftM FType parseType

parseType :: Parser Type
parseType = choice $ map try [parseScalar, parseConst]

parseScalar :: Parser Type
parseScalar = parens $ do
  _ <- string "scalar"
  sName <- spaces1 >> validName
  bTarget <- spaces1 >> parseBuiltInName

  return $ TScalar sName bTarget

parseConst :: Parser Type
parseConst = parens $ do
  _ <- string "const"
  sName <- spaces1 >> parseBuiltInName
  bValue <- spaces1 >> validNumber

  return $ TConst sName bValue

parseBuiltInName :: Parser BuiltIn
parseBuiltInName = liftM read $ choice names
  where
    names = map (try . string . show) bis
    bis = [minBound .. maxBound] :: [BuiltIn]
