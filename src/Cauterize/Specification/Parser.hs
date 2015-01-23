module Cauterize.Specification.Parser
  ( parseFile
  , parseString
  ) where

import Text.Parsec
import Text.Parsec.String

import Cauterize.Specification.Types
import Cauterize.Common.ParserUtils
import Cauterize.Common.Types

import Control.Monad

parseFile :: FilePath -> IO (Either ParseError Spec)
parseFile path = liftM (parseString path) $ readFile path

parseString :: FilePath -> String -> Either ParseError Spec
parseString path str = 
  case parse parseSpec path str of
     Left e -> Left e
     Right s -> Right s

parseSpec :: Parser Spec
parseSpec = do
  s <- pSpec
  spacedEof
  return s
  where
    pSpec = pSexp "specification" $ do
      qname <- spacedQuoted
      qver <- spacedQuoted
      qhash <- spacedFormHash
      sz <- parseRangeSize
      types <- pTypes
      return $ Spec qname qver qhash sz types
    pTypes :: Parser [SpType]
    pTypes = option [] $ do
      spaces1
      parseType `sepBy` spaces1

parseType :: Parser SpType
parseType = choice $ map try
  [ parseBuiltin
  , parseScalar
  , parseConst
  , parseArray
  , parseVector
  , parseStruct
  , parseSet
  , parseEnum
  , parsePad
  ]

parseBuiltin :: Parser SpType
parseBuiltin = pSexp "builtin" $ do
  bi <- liftM TBuiltIn spacedBuiltIn
  hs <- spacedFormHash
  sz <- parseFixedSize
  return $ BuiltIn bi hs sz

parseScalar :: Parser SpType
parseScalar = pSexp "scalar" $ do
  n <- spacedName
  hs <- spacedFormHash
  sz <- parseFixedSize
  bi <- spacedBuiltIn
  return $ Scalar (TScalar n bi) hs sz

parseConst :: Parser SpType
parseConst = pSexp "const" $ do
  n <- spacedName
  hs <- spacedFormHash
  sz <- parseFixedSize
  bi <- spacedBuiltIn
  v <- spacedNumber
  return $ Const (TConst n bi v) hs sz

parseArray :: Parser SpType
parseArray = pSexp "array" $ do
  n <- spacedName
  hs <- spacedFormHash
  sz <- parseRangeSize
  len <- spacedNumber
  t <- spacedName
  return $ Array (TArray n t len) hs sz

parseVector :: Parser SpType
parseVector = pSexp "vector" $ do
  n <- spacedName
  hs <- spacedFormHash
  sz <- parseRangeSize
  repr <- spaces1 >> parseLengthRepr
  len <- spacedNumber
  t <- spacedName
  return $ Vector (TVector n t len) hs sz repr

parseStruct :: Parser SpType
parseStruct = pSexp "struct" $ do
  n <- spacedName
  hs <- spacedFormHash
  sz <- parseRangeSize
  fs <- spaces1 >> parseFields
  return $ Struct (TStruct n fs) hs sz

parseSet :: Parser SpType
parseSet = pSexp "set" $ do
  n <- spacedName
  hs <- spacedFormHash
  sz <- parseRangeSize
  repr <- spaces1 >> parseFlagsRepr
  fs <- spaces1 >> parseFields
  return $ Set (TSet n fs) hs sz repr

parseEnum :: Parser SpType
parseEnum = pSexp "enum" $ do
  n <- spacedName
  hs <- spacedFormHash
  sz <- parseRangeSize
  repr <- spaces1 >> parseTagRepr
  fs <- spaces1 >> parseFields
  return $ Enum (TEnum n fs) hs sz repr

parsePad :: Parser SpType
parsePad = pSexp "pad" $ do
  n <- spacedName
  hs <- spacedFormHash
  sz <- parseFixedSize
  return $ Pad (TPad n (unFixedSize sz)) hs sz

parseFieldList :: Parser [Field]
parseFieldList = option [] $ do
      spaces1
      parseField `sepBy` spaces1

parseFixedSize :: Parser FixedSize
parseFixedSize = (>>) spaces1 $ pSexp "fixed-size" $ liftM FixedSize spacedNumber

parseRangeSize :: Parser RangeSize
parseRangeSize = (>>) spaces1 $ pSexp "range-size" $ liftM2 RangeSize spacedNumber spacedNumber

parseField :: Parser Field
parseField = pSexp "field" $ do
  n <- spacedName
  try (parseFullField n) <|> parseEmptyField n

parseFullField :: String -> Parser Field
parseFullField n = do
  t <- spacedName
  ix <- spacedNumber
  return $ Field n t ix
  
parseEmptyField :: String -> Parser Field
parseEmptyField n = do
  ix <- spacedNumber
  return $ EmptyField n ix
  
parseLengthRepr :: Parser LengthRepr
parseLengthRepr = pSexp "length-repr" $ liftM LengthRepr spacedBuiltIn
  
parseTagRepr :: Parser TagRepr
parseTagRepr = pSexp "tag-repr" $ liftM TagRepr spacedBuiltIn
  
parseFlagsRepr :: Parser FlagsRepr
parseFlagsRepr = pSexp "flags-repr" $ liftM FlagsRepr spacedBuiltIn

parseFields :: Parser Fields
parseFields = pSexp "fields" $ liftM Fields parseFieldList
