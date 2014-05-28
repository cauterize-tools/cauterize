module Cauterize.Schema.Parser
  ( parseFile
  , parseString
  , ASTSchema
  ) where

import Control.Monad

import Text.Parsec
import Text.Parsec.String

import Cauterize.Common.ParserUtils
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
  return $ Scalar $ TScalar n b

parseConst :: Parser ASTType
parseConst = pSexp "const" $ do
  n <- spacedName
  b <- spacedBuiltIn
  i <- spacedNumber
  return $ Const $ TConst n b i

parseFixedArray :: Parser ASTType
parseFixedArray = parseArr "fixed" (\n m i -> FixedArray $ TFixedArray n m i)

parseBoundedArray :: Parser ASTType
parseBoundedArray = parseArr "bounded" (\n m i -> BoundedArray $ TBoundedArray n m i)

parseArr :: String -> (Name -> Name -> Integer -> a) -> Parser a
parseArr identStr mkArrFn = pSexp identStr $ do
  n <- spacedName
  m <- spacedName
  i <- spacedNumber
  return $ mkArrFn n m i

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
  return $ Struct $ TStruct n (tagWithIndex rs)

parseEnum :: Parser ASTType
parseEnum = pSexp "enum" $ do
  n <- spacedName
  rs <- parseIndexedRefs
  return $ Enum $ TEnum n (tagWithIndex rs)

parseSet :: Parser ASTType
parseSet = pSexp "set" $ do
  n <- spacedName
  rs <- parseIndexedRefs 
  return $ Set $ TSet n (tagWithIndex rs)

parsePad :: Parser ASTType
parsePad = pSexp "pad" $ do
  n <- spacedName
  i <- spacedNumber
  return $ Pad $ TPad n i

parsePartial :: Parser ASTType
parsePartial = pSexp "partial" $ do
  n <- spacedName
  rs <- parseIndexedRefs
  return $ Partial $ TPartial n (tagWithIndex rs)

tagWithIndex :: (Enum a, Num a) => [a -> b] -> [b]
tagWithIndex rs = zipWith ($) rs [0..]
