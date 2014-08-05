module Cauterize.Schema.Parser
  ( parseFile
  , parseString
  , Schema
  ) where

import Control.Monad

import Text.Parsec
import Text.Parsec.String

import Cauterize.Common.ParserUtils
import Cauterize.Schema.Types

import Cauterize.Common.Primitives
import Cauterize.Common.Types
import Cauterize.Common.Field

parseFile :: FilePath -> IO (Either ParseError Schema)
parseFile path = readFile path >>= parseString path

parseString :: FilePath -> String -> IO (Either ParseError Schema)
parseString path str = 
  return $ case parse parseSchema path str of
              Left e -> Left e
              Right s -> Right s

parseSchema :: Parser Schema
parseSchema = do
  s <- pSchema
  spacedEof
  return s
  where
    pSchema = pSexp "schema" $ do
      qname <- spacedQuoted
      qver <- spacedQuoted
      forms <- pTypes
      return $ Schema qname qver (bis ++ forms)
    pTypes = option [] $ spaces1 >> parseType `sepBy` spaces1 
    bis = map (BuiltIn . TBuiltIn) [minBound .. maxBound]

parseType :: Parser ScType
parseType = choice $ map try
  [ parseScalar
  , parseConst
  , parseArray
  , parseVector
  , parseStruct
  , parseEnum
  , parseSet
  , parsePad
  ]

parseScalar :: Parser ScType
parseScalar = pSexp "scalar" $ do
  n <- spacedName
  b <- spacedBuiltIn
  return $ Scalar $ TScalar n b

parseConst :: Parser ScType
parseConst = pSexp "const" $ do
  n <- spacedName
  b <- spacedBuiltIn
  i <- spacedNumber
  return $ Const $ TConst n b i

parseArray :: Parser ScType
parseArray = parseArr "array" (\n m i -> Array $ TArray n m i)

parseVector :: Parser ScType
parseVector = parseArr "vector" (\n m i -> Vector $ TVector n m i)

parseArr :: String -> (Name -> Name -> Integer -> a) -> Parser a
parseArr identStr mkArrFn = pSexp identStr $ do
  n <- spacedName
  m <- spacedName
  i <- spacedNumber
  return $ mkArrFn n m i

parseField :: Parser (Integer -> Field)
parseField = pSexp "field" $ do
  n <- spacedName
  m <- optionMaybe spacedName
  return $ \i -> case m of
                    Just m' -> Field n m' i
                    Nothing -> EmptyField n i

parseSpacedFields :: Parser [Integer -> Field]
parseSpacedFields = many $ spaces1 >> parseField

parseStruct :: Parser ScType
parseStruct = pSexp "struct" $ do
  n <- spacedName
  fs <- spaces1 >> parseFields
  return $ Struct $ TStruct n fs

parseEnum :: Parser ScType
parseEnum = pSexp "enum" $ do
  n <- spacedName
  fs <- spaces1 >> parseFields
  return $ Enum $ TEnum n fs

parseSet :: Parser ScType
parseSet = pSexp "set" $ do
  n <- spacedName
  fs <- spaces1 >> parseFields 
  return $ Set $ TSet n fs

parsePad :: Parser ScType
parsePad = pSexp "pad" $ do
  n <- spacedName
  i <- spacedNumber
  return $ Pad $ TPad n i

tagWithIndex :: (Enum a, Num a) => [a -> b] -> [b]
tagWithIndex rs = zipWith ($) rs [0..]

parseFields :: Parser Fields
parseFields = pSexp "fields" $ liftM (Fields . tagWithIndex) parseSpacedFields
