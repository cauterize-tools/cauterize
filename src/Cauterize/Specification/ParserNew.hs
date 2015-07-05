{-# LANGUAGE OverloadedStrings, PatternSynonyms  #-}
module Cauterize.Specification.ParserNew
  ( parseSpecification
  ) where

import Control.Monad
import Data.SCargot.General
import Data.SCargot.Repr
import Data.SCargot.Repr.WellFormed
import Data.Text (Text, pack, unpack)
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Char

import Cauterize.Specification.TypesNew
import Cauterize.CommonTypesNew
import qualified Cauterize.HashNew as H

defaultSpecification :: Specification
defaultSpecification = Specification
  { specName = "specification"
  , specVersion = "0.0.0.0"
  , specFingerprint = H.hashNull
  , specSize = mkConstSize 1
  , specDepth = 0
  , specTypeLength = 0
  , specLengthTag = T1
  , specTypes = []
  }

data Atom
  = Number Integer
  | Ident Text
  | Str Text
  | Hash H.Hash
  | Tag Tag
  deriving (Show)

data Component
  = Name Text
  | Version Text
  | Fingerprint H.Hash
  | SpecSize Size
  | Depth Integer
  | TypeLength Integer
  | LengthTag Tag
  | TypeDef Type
  deriving (Show)

pAtom :: Parser Atom
pAtom = try pString <|> try pTag <|> try pHash <|> pNumber <|> pIdent
  where
    pString = do
      _ <- char '"'
      s <- many $ noneOf "\""
      _ <- char '"'
      (return . Str . pack) s
    pTag = do
      _ <- char 't'
      d <- oneOf "1248"
      return $ case d of
                '1' -> Tag T1
                '2' -> Tag T2
                '4' -> Tag T4
                '8' -> Tag T8
    pNumber = do
      sign <- option '+' (oneOf "-+")
      v <- fmap (read :: String -> Integer) (many1 digit)
      let v' = if sign == '-'
                then (-1) * v
                else v
      return (Number v')
    pIdent = do
      f <- oneOf ['a'..'z']
      r <- many $ oneOf ['a'..'z'] <|> digit <|> char '_'
      (return . Ident . pack) (f:r)
    pHash = let bytes = count 20 pHexByte
                htxt = liftM (pack . concat) bytes
            in liftM (Hash . H.mkHashFromHexString) htxt

    pHexByte = let pNibble = oneOf "abcdef0123456789"
               in count 2 pNibble

sAtom :: Atom -> Text
sAtom (Number n) = pack (show n)
sAtom (Ident i) = i
sAtom (Str s) = pack ("\"" ++ show s ++ "\"")
sAtom (Hash h) = H.hashToHex h

toComponent :: WellFormedSExpr Atom -> Either String Component
toComponent = asList go
  where
    go [A (Ident "name"), A (Str name)] = Right (Name name)
    go [A (Ident "version"), A (Str version)] = Right (Version version)
    go [A (Ident "fingerprint"), A (Hash h)] = Right (Fingerprint h)
    go [A (Ident "size"), A (Number smin), A (Number smax)] = Right (SpecSize $ mkSize smin smax)
    go [A (Ident "size"), A (Number sz)] = Right (SpecSize $ mkConstSize sz)
    go [A (Ident "depth"), A (Number d)] = Right (Depth d)
    go [A (Ident "typelength"), A (Number d)] = Right (TypeLength d)
    go [A (Ident "lengthtag"), A (Tag t)] = Right (LengthTag t)

    -- go (A (Ident "type") : rs ) = toType rs

    go (A (Ident x) : _ ) = Left ("Unhandled component: " ++ show x)
    go y = Left ("Invalid component: " ++ show y)

fromComponent :: Component -> WellFormedSExpr Atom
fromComponent c =
  case c of
    (Name n) -> L [ ident "name", A . Ident $ n ]
    (Version v) -> L [ ident "version", A . Ident $ v ]
    (Fingerprint h) -> L [ ident "fingerprint", hash h ]
    (SpecSize s) | sizeMin s == sizeMax s -> L [ ident "size", number (sizeMax s) ]
                 | otherwise -> L [ ident "size", number (sizeMin s), number (sizeMax s) ]
    (Depth d) -> L [ ident "depth", number d ]
    (TypeLength l) -> L [ ident "typelength", number l ]
    (LengthTag t) -> L [ ident "lengthtag", tag t ]

    -- (TypeDef t) -> fromType t

  where
    ident = A . Ident . pack
    hash = A . Hash
    number = A . Number
    tag = A . Tag

cauterizeSpec :: SExprSpec Atom Component
cauterizeSpec = convertSpec toComponent fromComponent $ asWellFormed $ mkSpec pAtom sAtom

componentsToSpec :: [Component] -> Specification
componentsToSpec = foldl go defaultSpecification
  where
    go s (Name n) = s { specName = n }
    go s (Version v) = s { specVersion = v }
    go s (Fingerprint h) = s { specFingerprint = h }
    go s (SpecSize sz) = s { specSize = sz }
    go s (Depth d) = s { specDepth = d }
    go s (TypeLength l) = s { specTypeLength = l }
    go s (LengthTag t) = s { specLengthTag = t }
    go s (TypeDef t) = let ts = specTypes s
                        in s { specTypes = t:ts }


parseSpecification :: String -> Either String Specification
parseSpecification t = componentsToSpec `fmap` decode cauterizeSpec (pack t)
