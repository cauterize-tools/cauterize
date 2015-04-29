module Cauterize.Lexer
  ( CauterizeP
  , identifier
  , charLiteral
  , natural
  , integer
  , float
  , symbol
  , lexeme
  , whiteSpace
  , parens
  ) where

import Control.Monad
import Data.Functor.Identity (Identity)
import Text.Parsec
import qualified Data.Text.Lazy as T
import qualified Text.Parsec.Token as TOK

type CauterizeP a = ParsecT T.Text () Identity a

lexer :: Monad m => TOK.GenTokenParser T.Text s m
lexer = TOK.makeTokenParser TOK.LanguageDef
  { TOK.commentStart = ""
  , TOK.commentEnd = ""
  , TOK.commentLine = ";;"
  , TOK.nestedComments = True
  , TOK.identStart = oneOf ['a'..'z']
  , TOK.identLetter = char '_' <|> oneOf ['a'..'z'] <|> oneOf ['0'..'9']
  , TOK.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , TOK.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , TOK.reservedNames = []
  , TOK.reservedOpNames = []
  , TOK.caseSensitive = True
  }

identifier :: CauterizeP T.Text
identifier = liftM T.pack $ TOK.identifier lexer

charLiteral :: CauterizeP Char
charLiteral = TOK.charLiteral lexer

natural :: CauterizeP Integer
natural = TOK.natural lexer

integer :: CauterizeP Integer
integer = TOK.integer lexer

float :: CauterizeP Double
float = TOK.float lexer

symbol :: String -> CauterizeP T.Text
symbol s = liftM T.pack $ TOK.symbol lexer s

lexeme :: CauterizeP a -> CauterizeP a
lexeme = TOK.lexeme lexer

whiteSpace :: CauterizeP ()
whiteSpace = TOK.whiteSpace lexer

parens :: CauterizeP a -> CauterizeP a
parens = TOK.parens lexer
