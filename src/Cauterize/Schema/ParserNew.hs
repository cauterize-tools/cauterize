{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
module Cauterize.Schema.ParserNew
  ( parseSchema
  , defaultSchema
  ) where

import Data.SCargot.General
import Data.SCargot.Repr
import Data.SCargot.Repr.WellFormed
import Data.Text (Text, pack, unpack)
import Text.Parsec
import Text.Parsec.Text

import Cauterize.Schema.TypesNew

defaultSchema :: Schema
defaultSchema = Schema
  { schemaName = "schema"
  , schemaVersion = "0.0.0.0"
  , schemaTypes = []
  }

data Atom
  = Number Integer
  | Ident Text
  | Str Text
  deriving (Show)

data Component
  = Name Text
  | Version Text
  | TypeDef Type
  deriving (Show)

pAtom :: Parser Atom
pAtom = try pString <|> pNumber <|> pIdent
  where
    pString = do
      _ <- char '"'
      s <- many $ noneOf "\""
      _ <- char '"'
      (return . Str . pack) s
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

sAtom :: Atom -> Text
sAtom (Ident t) = t
sAtom (Number n) = pack (show n)
sAtom (Str v) = pack ("\"" ++ show v ++ "\"")

toComponent :: WellFormedSExpr Atom -> Either String Component
toComponent = asList go
  where
    go [A (Ident "name"), A (Str name)] = Right (Name name)
    go [A (Ident "version"), A (Str version)] = Right (Version version)
    go (A (Ident "type") : rs ) = toType rs
    go (A (Ident x) : _ ) = Left ("Unhandled component: " ++ show x)
    go y = Left ("Not a component name: " ++ show y)

toType :: [WellFormedSExpr Atom] -> Either String Component
toType [] = Left "Empty type expression."
toType (tproto:tbody) =
  case tproto of
    A (Ident "synonym") -> toSynonym tbody
    A (Ident "range") -> toRange tbody
    A (Ident "array") -> toArray tbody
    A (Ident "vector") -> toVector tbody
    A (Ident "enumeration") -> toEnumeration tbody
    A (Ident "record") -> toRecord tbody
    A (Ident "combination") -> toCombination tbody
    A (Ident "union") -> toUnion tbody
    A (Ident x) -> Left ("Invalid prototype: " ++ show x)
    y -> Left ("Invalid type expression: " ++ show y)
  where
    umi = unsafeMkIdentifier . unpack
    mkTD n t = (Right . TypeDef) (Type (umi n) t)

    toField (L [A (Ident "field"), A (Ident name), A (Ident ref)]) =
      Right $ DataField (umi name) (umi ref)
    toField (L [A (Ident "empty"), A (Ident name)]) =
      Right $ EmptyField (umi name)
    toField x = Left ("Unexpected field body: " ++ show x)

    toSynonym [A (Ident name), A (Ident ref)] =
      mkTD name (Synonym (umi ref))
    toSynonym x = Left ("Unexpected synonym body: " ++ show x)

    toRange [A (Ident name), A (Number rmin), A (Number rmax)] =
      mkTD name (Range (fromIntegral rmin) (fromIntegral (rmax - rmin)))
    toRange x = Left ("Unexpected range body: " ++ show x)

    toArray [A (Ident name), A (Ident ref), A (Number size)] =
      mkTD name (Array (umi ref) (fromIntegral size))
    toArray x = Left ("Unexpected array body: " ++ show x)

    toVector [A (Ident name), A (Ident ref), A (Number size)] =
      mkTD name (Vector (umi ref) (fromIntegral size))
    toVector x = Left ("Unexpected vector body: " ++ show x)

    toEnumeration [A (Ident name), L (A (Ident "values"):vs)] = do
      let toValue (A (Ident val)) = Right (umi val)
          toValue x = Left ("Unexpected value: " ++ show x)
      vs' <- mapM toValue vs
      mkTD name (Enumeration vs')
    toEnumeration x = Left ("Unexpected enumeration body: " ++ show x)

    toRecord [A (Ident name), L (A (Ident "fields"):fs)] = do
      fs' <- mapM toField fs
      mkTD name (Record fs')
    toRecord x = Left ("Unexpected record body: " ++ show x)

    toCombination [A (Ident name), L (A (Ident "fields"):fs)] = do
      fs' <- mapM toField fs
      mkTD name (Combination fs')
    toCombination x = Left ("Unexpected combination body: " ++ show x)

    toUnion [A (Ident name), L (A (Ident "fields"):fs)] = do
      fs' <- mapM toField fs
      mkTD name (Union fs')
    toUnion x = Left ("Unexpected union body: " ++ show x)

fromComponent :: Component -> WellFormedSExpr Atom
fromComponent c =
  case c of
    (Name n) -> L [ ident "name", A . Ident $ n ]
    (Version v) -> L [ ident "version", A . Ident $ v ]
    (TypeDef _) -> L [ ident "type", ident "..." ]
  where
    ident = A . Ident . pack

cauterizeSpec :: SExprSpec Atom Component
cauterizeSpec = convertSpec toComponent fromComponent $ asWellFormed $ mkSpec pAtom sAtom

componentsToSchema :: [Component] -> Schema
componentsToSchema = foldl go defaultSchema
  where
    go s (Name n) = s { schemaName = n }
    go s (Version v) = s { schemaVersion = v }
    go s (TypeDef t) = let ts = schemaTypes s
                        in s { schemaTypes = t:ts }

parseSchema :: String -> Either String Schema
parseSchema t = componentsToSchema `fmap` decode cauterizeSpec (pack t)
