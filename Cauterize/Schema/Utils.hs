module Cauterize.Schema.Utils where

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
validName = many1 $ oneOf ['a'..'z']

validNumber :: Parser Integer
validNumber = do
  sign <- optionMaybe $ oneOf "+-"
  num <- liftM read $ many1 digit

  return $ case sign of
              Just '-' -> -1 * num
              _ -> num
