{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Common.ParserUtils
  ( pSexp
  , parens
  , spacedSchemaName
  , spacedSchemaVersion
  , spaces1
  , spacedName
  , spacedNumber
  , spacedEof
  , spacedFormHash
  , spacedBuiltIn
  ) where

import Text.Parsec
import Text.Parsec.Text.Lazy

import Cauterize.FormHash

import Control.Monad

import Data.Word
import qualified Data.Text.Lazy as T
import Numeric

import qualified Data.ByteString as BS

import Cauterize.Common.Types

parens :: Parser a -> Parser a
parens a = do
  _ <- char '('
  a' <- a
  _ <- char ')'
  return a'

parseManyStartingWith :: String -> String -> Parser T.Text
parseManyStartingWith s r = do
  s' <- oneOf s
  r' <- many . oneOf $ r
  return $ s' `T.cons` T.pack r'

schemaName :: Parser T.Text
schemaName = parseManyStartingWith start rest
  where
    start = ['a'..'z']
    rest = start ++ "_" ++ ['0'..'9']

schemaVersion :: Parser T.Text
schemaVersion = parseManyStartingWith start rest
  where
    start = ['a'..'z'] ++ ['0'..'9']
    rest = start ++ "_.-"

spaces1 :: Parser ()
spaces1 = space >> spaces

validName :: Parser T.Text
validName = parseManyStartingWith start rest
  where
    start = ['a'..'z']
    rest = start ++ "_" ++ ['0'..'9']

validNumber :: Parser Integer
validNumber = do
  sign <- optionMaybe $ oneOf "+-"
  num <- liftM read $ many1 digit

  return $ case sign of
              Just '-' -> -1 * num
              _ -> num

parseFormHash :: Parser FormHash
parseFormHash = pSexp "sha1" $ do
  spaces1
  liftM (FormHash . BS.pack . hexStrToWord8s) $ count 40 $ oneOf (['a'..'f'] ++ ['0'..'9'])

spacedName :: Parser T.Text
spacedName = spaces1 >> validName

spacedNumber :: Parser Integer
spacedNumber = spaces1 >> validNumber

spacedSchemaName :: Parser T.Text
spacedSchemaName = spaces1 >> schemaName

spacedSchemaVersion :: Parser T.Text
spacedSchemaVersion = spaces1 >> schemaVersion

spacedFormHash :: Parser FormHash
spacedFormHash = spaces1 >> parseFormHash

pSexp :: String -> Parser a -> Parser a
pSexp n p = parens $ string n >> p

spacedEof :: Parser ()
spacedEof = spaces >> eof

hexStrToWord8s :: String -> [Word8]
hexStrToWord8s (a:b:rs) = let w = (fst . head) $ readHex [a,b]
                              rs' = hexStrToWord8s rs
                          in w:rs'
hexStrToWord8s [] = []
hexStrToWord8s _ = error "Must have even number of characters"

parseBuiltInName :: Parser BuiltIn
parseBuiltInName = liftM read $ choice names
  where
    names = map (try . string . show) bis
    bis = [minBound .. maxBound] :: [BuiltIn]

spacedBuiltIn :: Parser BuiltIn
spacedBuiltIn = spaces1 >> parseBuiltInName
