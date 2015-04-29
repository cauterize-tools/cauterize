module Cauterize.Specification.Parser
  ( parseFile
  , parseText
  ) where

import Text.Parsec
import Text.Parsec.Text.Lazy

import Cauterize.Lexer
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
  whiteSpace
  s <- pSpec
  eof
  return s
  where
    pSpec = pSexp "specification" $ do
      qname <- identifier
      qver <- parseSchemaVersion
      qhash <- parseFormHash
      sz <- parseRangeSize
      depth <- parseDepth
      tyTag <- parseTypeTagWidth
      lnTag <- parseLengthTagWidth
      types <- pTypes
      return $ Spec qname qver qhash sz depth tyTag lnTag types
    pTypes :: Parser [SpType]
    pTypes = option [] $ many parseType

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
  bi <- liftM TBuiltIn parseBuiltInName
  hs <- parseFormHash
  sz <- parseFixedSize
  return $ BuiltIn bi hs sz

parseSynonym :: Parser SpType
parseSynonym = pSexp "synonym" $ do
  n <- identifier
  hs <- parseFormHash
  sz <- parseFixedSize
  bi <- parseBuiltInName
  return $ Synonym (TSynonym n bi) hs sz

parseArray :: Parser SpType
parseArray = pSexp "array" $ do
  n <- identifier
  hs <- parseFormHash
  sz <- parseRangeSize
  len <- natural
  t <- identifier
  return $ Array (TArray n t len) hs sz

parseVector :: Parser SpType
parseVector = pSexp "vector" $ do
  n <- identifier
  hs <- parseFormHash
  sz <- parseRangeSize
  repr <- parseLengthRepr
  len <- natural
  t <- identifier
  return $ Vector (TVector n t len) hs sz repr

parseRecord :: Parser SpType
parseRecord = pSexp "record" $ do
  n <- identifier
  hs <- parseFormHash
  sz <- parseRangeSize
  fs <- parseFields
  return $ Record (TRecord n fs) hs sz

parseCombination :: Parser SpType
parseCombination = pSexp "combination" $ do
  n <- identifier
  hs <- parseFormHash
  sz <- parseRangeSize
  repr <- parseFlagsRepr
  fs <- parseFields
  return $ Combination (TCombination n fs) hs sz repr

parseUnion :: Parser SpType
parseUnion = pSexp "union" $ do
  n <- identifier
  hs <- parseFormHash
  sz <- parseRangeSize
  repr <- parseTagRepr
  fs <- parseFields
  return $ Union (TUnion n fs) hs sz repr

parseFieldList :: Parser [Field]
parseFieldList = option [] $ many parseField

parseFixedSize :: Parser FixedSize
parseFixedSize = pSexp "fixed-size" $ liftM FixedSize natural

parseRangeSize :: Parser RangeSize
parseRangeSize = pSexp "range-size" $ liftM2 RangeSize natural natural

parseDepth :: Parser Depth
parseDepth = pSexp "depth" (liftM Depth natural)

parseTypeTagWidth :: Parser TypeTagWidth
parseTypeTagWidth = pSexp "type-width" (liftM TypeTagWidth natural)

parseLengthTagWidth :: Parser LengthTagWidth
parseLengthTagWidth = pSexp "length-width" (liftM LengthTagWidth natural)

parseField :: Parser Field
parseField = pSexp "field" $ do
  n <- identifier
  try (parseFullField n) <|> parseEmptyField n

parseFullField :: T.Text -> Parser Field
parseFullField n = do
  t <- identifier
  ix <- natural
  return $ Field n t ix

parseEmptyField :: T.Text -> Parser Field
parseEmptyField n = liftM (EmptyField n) natural

parseLengthRepr :: Parser LengthRepr
parseLengthRepr = pSexp "length-repr" $ liftM LengthRepr parseBuiltInName

parseTagRepr :: Parser TagRepr
parseTagRepr = pSexp "tag-repr" $ liftM TagRepr parseBuiltInName

parseFlagsRepr :: Parser FlagsRepr
parseFlagsRepr = pSexp "flags-repr" $ liftM FlagsRepr parseBuiltInName

parseFields :: Parser Fields
parseFields = pSexp "fields" $ liftM Fields parseFieldList
