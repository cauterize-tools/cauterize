{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Schema.Parser
  ( parseFile
  , parseText
  , Schema
  ) where

import Control.Monad

import Text.Parsec
import Text.Parsec.Text.Lazy

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Cauterize.Lexer
import Cauterize.Common.ParserUtils
import Cauterize.Common.Types
import Cauterize.Schema.Types

parseFile :: FilePath -> IO (Either ParseError Schema)
parseFile path = liftM (parseText path) $ T.readFile path

parseText :: FilePath -> T.Text -> Either ParseError Schema
parseText path str =
  case parse parseSchema path str of
     Left e -> Left e
     Right s -> Right s

parseSchema :: Parser Schema
parseSchema = do
  whiteSpace
  s <- pSchema
  eof
  return s
  where
    pSchema = pSexp "schema" $ do
      qname <- identifier
      qver <- parseSchemaVersion
      forms <- pTypes
      return $ Schema qname qver (bis ++ forms)
    pTypes = option [] $ many parseType
    bis = map (BuiltIn . TBuiltIn) [minBound .. maxBound]

parseType :: Parser ScType
parseType = choice $ map try
  [ parseSynonym
  , parseArray
  , parseVector
  , parseRecord
  , parseUnion
  , parseCombination
  ]

parseSynonym :: Parser ScType
parseSynonym = pSexp "synonym" $ do
  n <- identifier
  b <- parseBuiltInName
  return $ Synonym $ TSynonym n b

parseArray :: Parser ScType
parseArray = parseArr "array" (\n m i -> Array $ TArray n m i)

parseVector :: Parser ScType
parseVector = parseArr "vector" (\n m i -> Vector $ TVector n m i)

parseArr :: String -> (Name -> Name -> Integer -> a) -> Parser a
parseArr identStr mkArrFn = pSexp identStr $ do
  n <- identifier
  m <- identifier
  i <- natural
  return $ mkArrFn n m i

parseField :: Parser (Integer -> Field)
parseField = pSexp "field" $ do
  n <- identifier
  m <- optionMaybe identifier
  return $ \i -> case m of
                    Just m' -> Field n m' i
                    Nothing -> EmptyField n i

parseSpacedFields :: Parser [Integer -> Field]
parseSpacedFields = many parseField

parseRecord :: Parser ScType
parseRecord = pSexp "record" $ do
  n <- identifier
  fs <- parseFields
  return $ Record $ TRecord n fs

parseUnion :: Parser ScType
parseUnion = pSexp "union" $ do
  n <- identifier
  fs <- parseFields
  return $ Union $ TUnion n fs

parseCombination :: Parser ScType
parseCombination = pSexp "combination" $ do
  n <- identifier
  fs <- parseFields
  return $ Combination $ TCombination n fs

tagWithIndex :: (Enum a, Num a) => [a -> b] -> [b]
tagWithIndex rs = zipWith ($) rs [0..]

parseFields :: Parser Fields
parseFields = pSexp "fields" $ liftM (Fields . tagWithIndex) parseSpacedFields
