{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Common.ParserUtils
  ( pSexp
  , parseBuiltInName
  , parseSchemaVersion
  , parseFormHash
  ) where

import Text.Parsec
import Text.Parsec.Text.Lazy

import Cauterize.FormHash
import Cauterize.Lexer

import Control.Monad

import Data.Word
import qualified Data.Text.Lazy as T
import Numeric

import qualified Data.ByteString as BS

import Cauterize.Common.Types

parseManyStartingWith :: String -> String -> Parser T.Text
parseManyStartingWith s r = lexeme $ do
  s' <- oneOf s
  r' <- many . oneOf $ r
  return $ s' `T.cons` T.pack r'

parseSchemaVersion :: Parser T.Text
parseSchemaVersion = parseManyStartingWith start rest
  where
    start = ['a'..'z'] ++ ['0'..'9']
    rest = start ++ "_.-"

parseFormHash :: Parser FormHash
parseFormHash = pSexp "sha1" $
  lexeme $ liftM (FormHash . BS.pack . hexStrToWord8s) $ count 40 $ oneOf (['a'..'f'] ++ ['0'..'9'])

pSexp :: String -> Parser a -> Parser a
pSexp n p = lexeme (parens $ reserved n >> p)

hexStrToWord8s :: String -> [Word8]
hexStrToWord8s (a:b:rs) = let w = (fst . head) $ readHex [a,b]
                              rs' = hexStrToWord8s rs
                          in w:rs'
hexStrToWord8s [] = []
hexStrToWord8s _ = error "Must have even number of characters"

parseBuiltInName :: Parser BuiltIn
parseBuiltInName = lexeme $ liftM read $ choice names
  where
    names = map (try . string . show) bis
    bis = [minBound .. maxBound] :: [BuiltIn]
