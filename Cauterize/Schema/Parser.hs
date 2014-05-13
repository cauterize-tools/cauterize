module Cauterize.Schema.Parser
  ( parseFile
  , parseString
  , ASTSchema
  ) where

import Control.Monad

import Text.Parsec
import Text.Parsec.String

import Cauterize.Schema.Utils
import Cauterize.Schema.Types

import Cauterize.Common.Primitives
import Cauterize.Common.Types
import Cauterize.Common.IndexedRef

type ASTSchema = Schema String
type ASTType = ScType String
type ASTIndexedRef = IndexedRef String

parseFile :: FilePath -> IO (Either ParseError ASTSchema)
parseFile path = readFile path >>= parseString path

parseString :: FilePath -> String -> IO (Either ParseError ASTSchema)
parseString path str = 
  return $ case parse parseSchema path str of
              Left e -> Left e
              Right s -> Right s

parseSchema :: Parser ASTSchema
parseSchema = pSexp "schema" $ do
    qname <- spacedQuoted
    qver <- spacedQuoted
    forms <- pForms
    return $ Schema qname qver (bis ++ forms)
  where
    pForms = option [] $ spaces1 >> parseForm `sepBy` spaces1 
    bis = map (FType . ScBuiltIn . TBuiltIn) [minBound .. maxBound]

parseForm :: Parser (SchemaForm String)
parseForm = liftM FType parseType

parseType :: Parser ASTType
parseType = choice $ map try
  [ parseScalar
  , parseConst
  , parseFixedArray
  , parseBoundedArray
  , parseStruct
  , parseEnum
  , parseSet
  , parsePad
  , parsePartial
  ]

parseScalar :: Parser ASTType
parseScalar = pSexp "scalar" $ do
  n <- spacedName
  b <- spacedBuiltIn
  return $ ScScalar $ TScalar n b

parseConst :: Parser ASTType
parseConst = pSexp "const" $ do
  n <- spacedName
  b <- spacedBuiltIn
  i <- spacedNumber
  return $ ScConst $ TConst n b i

parseFixedArray :: Parser ASTType
parseFixedArray = pSexp "fixed" $ do
  n <- spacedName
  m <- spacedName
  i <- spacedNumber
  return $ ScFixedArray $ TFixedArray n m i

parseBoundedArray :: Parser ASTType
parseBoundedArray = pSexp "bounded" $ do
  n <- spacedName
  m <- spacedName
  i <- spacedNumber
  return $ ScBoundedArray $ TBoundedArray n m i

parseIndexedRef :: Parser (Integer -> ASTIndexedRef)
parseIndexedRef = pSexp "field" $ do
  n <- spacedName
  m <- option "void" spacedName
  return $ \i -> IndexedRef n m i

parseIndexedRefs :: Parser [Integer -> ASTIndexedRef]
parseIndexedRefs = many $ spaces1 >> parseIndexedRef

parseStruct :: Parser ASTType
parseStruct = pSexp "struct" $ do
  n <- spacedName
  rs <- parseIndexedRefs
  return $ ScStruct $ TStruct n (tagWithIndex rs)

parseEnum :: Parser ASTType
parseEnum = pSexp "enum" $ do
  n <- spacedName
  rs <- parseIndexedRefs
  return $ ScEnum $ TEnum n (tagWithIndex rs)

parseSet :: Parser ASTType
parseSet = pSexp "set" $ do
  n <- spacedName
  rs <- parseIndexedRefs 
  return $ ScSet $ TSet n (tagWithIndex rs)

parsePad :: Parser ASTType
parsePad = pSexp "pad" $ do
  n <- spacedName
  i <- spacedNumber
  return $ ScPad $ TPad n i

parsePartial :: Parser ASTType
parsePartial = pSexp "partial" $ do
  n <- spacedName
  rs <- parseIndexedRefs
  return $ ScPartial $ TPartial n (tagWithIndex rs)

tagWithIndex :: (Enum a, Num a) => [a -> b] -> [b]
tagWithIndex rs = zipWith ($) rs [0..]

parseBuiltInName :: Parser BuiltIn
parseBuiltInName = liftM read $ choice names
  where
    names = map (try . string . show) bis
    bis = [minBound .. maxBound] :: [BuiltIn]

spacedBuiltIn :: Parser BuiltIn
spacedBuiltIn = spaces1 >> parseBuiltInName
