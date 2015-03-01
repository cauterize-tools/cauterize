module Cauterize.Specification.Parser
  ( parseFile
  , parseText
  ) where

import Text.Parsec
import Text.Parsec.Text.Lazy

import Cauterize.Specification.Types
import Cauterize.Common.ParserUtils
import Cauterize.Common.Types

import Control.Monad

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

parseFile :: FilePath -> IO (Either ParseError Spec)
parseFile path = liftM (parseText path) $ T.readFile path

parseText :: FilePath -> T.Text -> Either ParseError Spec
parseText path str =
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
      qname <- spacedSchemaName
      qver <- spacedSchemaVersion
      qhash <- spacedFormHash
      sz <- parseRangeSize
      depth <- parseDepth
      types <- pTypes
      return $ Spec qname qver qhash sz depth types
    pTypes :: Parser [SpType]
    pTypes = option [] $ do
      spaces1
      parseType `sepBy` spaces1

parseType :: Parser SpType
parseType = choice $ map try
  [ parseBuiltin
  , parseSynonym
  , parseArray
  , parseVector
  , parseRecord
  , parseCombination
  , parseUnion
  ]

parseBuiltin :: Parser SpType
parseBuiltin = pSexp "builtin" $ do
  bi <- liftM TBuiltIn spacedBuiltIn
  hs <- spacedFormHash
  sz <- parseFixedSize
  return $ BuiltIn bi hs sz

parseSynonym :: Parser SpType
parseSynonym = pSexp "synonym" $ do
  n <- spacedName
  hs <- spacedFormHash
  sz <- parseFixedSize
  bi <- spacedBuiltIn
  return $ Synonym (TSynonym n bi) hs sz

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

parseRecord :: Parser SpType
parseRecord = pSexp "record" $ do
  n <- spacedName
  hs <- spacedFormHash
  sz <- parseRangeSize
  fs <- spaces1 >> parseFields
  return $ Record (TRecord n fs) hs sz

parseCombination :: Parser SpType
parseCombination = pSexp "combination" $ do
  n <- spacedName
  hs <- spacedFormHash
  sz <- parseRangeSize
  repr <- spaces1 >> parseFlagsRepr
  fs <- spaces1 >> parseFields
  return $ Combination (TCombination n fs) hs sz repr

parseUnion :: Parser SpType
parseUnion = pSexp "union" $ do
  n <- spacedName
  hs <- spacedFormHash
  sz <- parseRangeSize
  repr <- spaces1 >> parseTagRepr
  fs <- spaces1 >> parseFields
  return $ Union (TUnion n fs) hs sz repr

parseFieldList :: Parser [Field]
parseFieldList = option [] $ do
      spaces1
      parseField `sepBy` spaces1

parseFixedSize :: Parser FixedSize
parseFixedSize = (>>) spaces1 $ pSexp "fixed-size" $ liftM FixedSize spacedNumber

parseRangeSize :: Parser RangeSize
parseRangeSize = (>>) spaces1 $ pSexp "range-size" $ liftM2 RangeSize spacedNumber spacedNumber

parseDepth :: Parser Depth
parseDepth = (>>) spaces1 $ pSexp "depth" (liftM Depth spacedNumber)

parseField :: Parser Field
parseField = pSexp "field" $ do
  n <- spacedName
  try (parseFullField n) <|> parseEmptyField n

parseFullField :: T.Text -> Parser Field
parseFullField n = do
  t <- spacedName
  ix <- spacedNumber
  return $ Field n t ix

parseEmptyField :: T.Text -> Parser Field
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
