module Cauterize.Common.ParserUtils
  ( pSexp
  , spacedQuoted
  , spaces1
  , spacedName
  , spacedNumber
  , spacedEof
  , spacedFormHash
  , spacedBuiltIn
  ) where

import Text.Parsec
import Text.Parsec.String

import Cauterize.FormHash

import Control.Monad

import Data.Word
import Numeric

import qualified Data.ByteString as BS

import Cauterize.Common.Types

parens :: Parser a -> Parser a
parens a = do
  _ <- char '('
  a' <- a
  _ <- char ')'
  return a'

quoted :: Parser String
quoted = do
  _ <- char '"'
  manyTill anyToken (char '"')

spaces1 :: Parser ()
spaces1 = space >> spaces

validName :: Parser String
validName = do
  f <- oneOf chars
  r <- many $ oneOf $ chars ++ digits
  return $ f:r
  where
    chars = ['a'..'z'] ++ ['A'..'Z']
    digits = ['0'..'9']

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
  liftM (FormHash . BS.pack . hexStrToWord8s) $ count 40 $ oneOf (['A'..'F'] ++ ['0'..'9'])

spacedName :: Parser String
spacedName = spaces1 >> validName

spacedNumber :: Parser Integer
spacedNumber = spaces1 >> validNumber

spacedQuoted :: Parser String
spacedQuoted = spaces1 >> quoted

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
