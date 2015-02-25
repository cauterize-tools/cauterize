module Cauterize.Schema.Parser
  ( parseFile
  , parseString
  , Schema
  ) where

import Control.Monad

import Text.Parsec
import Text.Parsec.String

import Cauterize.Common.ParserUtils
import Cauterize.Common.Types
import Cauterize.Schema.Types

parseFile :: FilePath -> IO (Either ParseError Schema)
parseFile path = liftM (parseString path) $ readFile path

parseString :: FilePath -> String -> Either ParseError Schema
parseString path str =
  case parse parseSchema path str of
     Left e -> Left e
     Right s -> Right s

parseSchema :: Parser Schema
parseSchema = do
  s <- pSchema
  spacedEof
  return s
  where
    pSchema = pSexp "schema" $ do
      qname <- spacedSchemaName
      qver <- spacedSchemaVersion
      forms <- pTypes
      return $ Schema qname qver (bis ++ forms)
    pTypes = option [] $ spaces1 >> parseType `sepBy` spaces1
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
  n <- spacedName
  b <- spacedBuiltIn
  return $ Synonym $ TSynonym n b

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

parseRecord :: Parser ScType
parseRecord = pSexp "record" $ do
  n <- spacedName
  fs <- spaces1 >> parseFields
  return $ Record $ TRecord n fs

parseUnion :: Parser ScType
parseUnion = pSexp "union" $ do
  n <- spacedName
  fs <- spaces1 >> parseFields
  return $ Union $ TUnion n fs

parseCombination :: Parser ScType
parseCombination = pSexp "combination" $ do
  n <- spacedName
  fs <- spaces1 >> parseFields
  return $ Combination $ TCombination n fs

tagWithIndex :: (Enum a, Num a) => [a -> b] -> [b]
tagWithIndex rs = zipWith ($) rs [0..]

parseFields :: Parser Fields
parseFields = pSexp "fields" $ liftM (Fields . tagWithIndex) parseSpacedFields
