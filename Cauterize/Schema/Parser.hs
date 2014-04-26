module Cauterize.Schema.Parser (parseFile) where

import Control.Monad

import Text.Parsec
import Text.Parsec.String

import Cauterize.Schema.Types
import Cauterize.Schema.Utils

parseFile :: FilePath -> IO (Either ParseError Schema)
parseFile path = readFile path >>= parseString path

parseString :: FilePath -> String -> IO (Either ParseError Schema)
parseString path str = 
  return $ case parse parseSchema path str of
              Left e -> Left e
              Right s -> Right s

parseSchema :: Parser Schema
parseSchema = pSexp "schema" pSchema
  where
    pSchema = liftM3 Schema spacedQuoted spacedQuoted pForms
    pForms = option [] $ spaces1 >> parseForm `sepBy` spaces1 

parseForm :: Parser Form
parseForm = liftM FType parseType

parseType :: Parser Type
parseType = choice $ map try
  [ parseScalar
  , parseConst
  , parseFixedArray
  , parseBoundedArray
  , parseStruct
  , parseEnum
  ]

parseScalar :: Parser Type
parseScalar = pSexp "scalar" $ liftM2 TScalar spacedName spacedBuiltIn

parseConst :: Parser Type
parseConst = pSexp "const" $ liftM3 TConst spacedName spacedBuiltIn spacedNumber

parseFixedArray :: Parser Type
parseFixedArray = pSexp "fixed" $ liftM3 TFixedArray spacedName spacedName spacedNumber

parseBoundedArray :: Parser Type
parseBoundedArray = pSexp "bounded" $ liftM3 TBoundedArray spacedName spacedName spacedNumber

parseStruct :: Parser Type
parseStruct = pSexp "struct" $ liftM2 TStruct spacedName parseFields
  where
    parseFields = many $ spaces1 >> parseField
    parseField = pSexp "field" $ liftM2 StructField spacedName spacedName

parseEnum :: Parser Type
parseEnum = pSexp "enum" $ liftM2 TEnum spacedName parseVariants
  where
    parseVariants = many $ spaces1 >> parseVariant
    parseVariant = pSexp "var" $ liftM2 EnumVariants spacedName (optionMaybe spacedName)




parseBuiltInName :: Parser BuiltIn
parseBuiltInName = liftM read $ choice names
  where
    names = map (try . string . show) bis
    bis = [minBound .. maxBound] :: [BuiltIn]

spacedBuiltIn :: Parser BuiltIn
spacedBuiltIn = spaces1 >> parseBuiltInName
