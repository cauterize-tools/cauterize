module Cauterize.Schema.Utils
  ( pSexp
  , spacedQuoted
  , spaces1
  , spacedName
  , spacedNumber
  ) where

import Text.Parsec
import Text.Parsec.String

import Control.Monad

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

spacedName :: Parser String
spacedName = spaces1 >> validName

spacedNumber :: Parser Integer
spacedNumber = spaces1 >> validNumber

spacedQuoted :: Parser String
spacedQuoted = spaces1 >> quoted

pSexp :: String -> Parser a -> Parser a
pSexp n p = parens $ string n >> p
