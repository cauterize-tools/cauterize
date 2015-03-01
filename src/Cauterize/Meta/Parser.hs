{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Meta.Parser where

import Text.Parsec
import Text.Parsec.Text.Lazy
import Data.Word
import Numeric

import Cauterize.Meta.Types
import Cauterize.Common.ParserUtils

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

parseFile :: FilePath -> IO (Either ParseError Meta)
parseFile path = T.readFile path >>= parseText path

parseText :: FilePath -> T.Text -> IO (Either ParseError Meta)
parseText path str =
  return $ case parse parseMeta path str of
              Left e -> Left e
              Right s -> Right s

parseMeta :: Parser Meta
parseMeta = do
  a <- pMeta
  spaces >> eof
  return a

pMeta :: Parser Meta
pMeta = parens $ do
  _ <- string "meta-interface"
  n <- spacedSchemaName
  sv <- spacedSchemaVersion
  sh <- spacedFormHash
  av <- spaces1 >> pMetaVariant
  tl <- spaces1 >> pMetaTypeLength
  dl <- spaces1 >> pMetaDataLength
  ts <- spaces1 >> pMetaTypes

  return $ Meta n av tl dl sh sv ts

pMetaVariant :: Parser Integer
pMetaVariant = pNamedNumber "meta-variant"

pMetaTypeLength :: Parser Integer
pMetaTypeLength = pNamedNumber "type-length"

pMetaDataLength :: Parser Integer
pMetaDataLength = pNamedNumber "data-length"

pNamedNumber :: String -> Parser Integer
pNamedNumber n = parens $ string n >> spacedNumber

pMetaTypes :: Parser [MetaType]
pMetaTypes = parens $ string "types" >> spaces1 >> sepBy1 pMetaType spaces1

pMetaType :: Parser MetaType
pMetaType = parens $ do
  _ <- string "type"
  n <- spacedName
  spaces1
  p <- pHexBytes
  return $ MetaType n p

pHexBytes :: Parser [Word8]
pHexBytes = many1 pHexByte

pHexByte :: Parser Word8
pHexByte = do
  c0 <- oneOf hexDigits
  c1 <- oneOf hexDigits
  let [(v,"")] = readHex [c0,c1]
  return v

hexDigits :: String
hexDigits = ['0'..'9'] ++ ['a'..'f']
