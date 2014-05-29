module Cauterize.Specification.Parser
  ( parseFile
  , parseString
  ) where

import Text.Parsec
import Text.Parsec.String

import Cauterize.Specification.Types
import Cauterize.Common.ParserUtils
import Cauterize.Common.Primitives
import Cauterize.Common.Types
import Cauterize.Common.IndexedRef

import Control.Monad

type ASTSpec = Spec Name
type ASTType = SpType Name

parseFile :: FilePath -> IO (Either ParseError ASTSpec)
parseFile path = liftM (parseString path) $ readFile path

parseString :: FilePath -> String -> Either ParseError ASTSpec
parseString path str = 
  case parse parseSpec path str of
     Left e -> Left e
     Right s -> Right s

parseSpec :: Parser ASTSpec
parseSpec = do
  s <- pSpec
  spacedEof
  return s
  where
    pSpec = pSexp "specification" $ do
      qname <- spacedQuoted
      qver <- spacedQuoted
      sz <- parseRangeSize
      qhash <- spacedFormHash
      types <- pTypes
      return $ Spec qname qver qhash sz types
    pTypes :: Parser [ASTType]
    pTypes = option [] $ do
      spaces1
      parseType `sepBy` spaces1

parseType :: Parser ASTType
parseType = choice $ map try
  [ parseBuiltin
  , parseScalar
  , parseConst
  , parseFixedArray
  , parseBoundedArray
  , parseStruct
  , parseSet
  , parseEnum
  , parsePartial
  , parsePad
  ]

parseBuiltin :: Parser ASTType
parseBuiltin = pSexp "builtin" $ do
  bi <- liftM TBuiltIn spacedBuiltIn
  hs <- spacedFormHash
  sz <- parseFixedSize
  return $ BuiltIn bi hs sz

parseScalar :: Parser ASTType
parseScalar = pSexp "scalar" $ do
  n <- spacedName
  hs <- spacedFormHash
  bi <- spacedBuiltIn
  sz <- parseFixedSize
  return $ Scalar (TScalar n bi) hs sz

parseConst :: Parser ASTType
parseConst = pSexp "const" $ do
  n <- spacedName
  hs <- spacedFormHash
  v <- spacedNumber
  bi <- spacedBuiltIn
  sz <- parseFixedSize
  return $ Const (TConst n bi v) hs sz

parseFixedArray :: Parser ASTType
parseFixedArray = pSexp "fixed" $ do
  n <- spacedName
  hs <- spacedFormHash
  t <- spacedName
  len <- spacedNumber
  sz <- parseRangeSize
  return $ FixedArray (TFixedArray n t len) hs sz

parseBoundedArray :: Parser ASTType
parseBoundedArray = pSexp "bounded" $ do
  n <- spacedName
  hs <- spacedFormHash
  t <- spacedName
  len <- spacedNumber
  repr <- spacedBuiltIn
  sz <- parseRangeSize
  return $ BoundedArray (TBoundedArray n t len) hs sz repr

parseStruct :: Parser ASTType
parseStruct = pSexp "struct" $ do
  n <- spacedName
  sz <- parseRangeSize
  hs <- spacedFormHash
  fs <- parseIndexedRefs
  return $ Struct (TStruct n fs) hs sz

parseSet :: Parser ASTType
parseSet = pSexp "set" $ do
  n <- spacedName
  sz <- parseRangeSize
  repr <- spacedBuiltIn
  hs <- spacedFormHash
  fs <- parseIndexedRefs
  return $ Set (TSet n fs) hs sz repr

parseEnum :: Parser ASTType
parseEnum = pSexp "enum" $ do
  n <- spacedName
  sz <- parseRangeSize
  repr <- spacedBuiltIn
  hs <- spacedFormHash
  fs <- parseIndexedRefs
  return $ Enum (TEnum n fs) hs sz repr

parsePartial :: Parser ASTType
parsePartial = pSexp "partial" $ do
  n <- spacedName
  sz <- parseRangeSize
  pTagRepr <- spacedBuiltIn
  pLenRepr <- spacedBuiltIn
  hs <- spacedFormHash
  fs <- parseIndexedRefs
  return $ Partial (TPartial n fs) hs sz pTagRepr pLenRepr

parsePad :: Parser ASTType
parsePad = pSexp "pad" $ do
  n <- spacedName
  hs <- spacedFormHash
  sz <- parseFixedSize
  return $ Pad (TPad n (unFixedSize sz)) hs sz

parseIndexedRefs :: Parser [IndexedRef Name]
parseIndexedRefs = option [] $ do
      spaces1
      parseIndexedRef `sepBy` spaces1

parseFixedSize :: Parser FixedSize
parseFixedSize = (>>) spaces1 $ pSexp "fixed-size" $ liftM FixedSize spacedNumber

parseRangeSize :: Parser RangeSize
parseRangeSize = (>>) spaces1 $ pSexp "range-size" $ liftM2 RangeSize spacedNumber spacedNumber

parseIndexedRef :: Parser (IndexedRef Name)
parseIndexedRef = pSexp "field" $ do
  n <- spacedName
  t <- spacedName
  ix <- spacedNumber
  return $ IndexedRef n t ix
  
