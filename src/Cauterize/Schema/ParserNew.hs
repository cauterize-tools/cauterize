{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
module Cauterize.Schema.ParserNew
  ( parseSchema

  , Schema(..)
  , Type(..)
  , TypeDesc(..)
  , Prim(..)
  , Field(..)
  , Offset
  , Size
  , Identifier, mkIdentifier

  ) where

import Data.Char
import Data.Int
import Data.Maybe
import Data.SCargot.General
import Data.SCargot.Repr
import Data.SCargot.Repr.WellFormed
import Data.String
import Data.Text (Text, pack, empty)
import Data.Word
import Text.Parsec
import Text.Parsec.Text

data Schema = Schema
  { schemaName :: Text
  , schemaVersion :: Text
  , schemaTypes :: [Type]
  } deriving (Show)

defaultSchema :: Schema
defaultSchema = Schema
  { schemaName = "schema"
  , schemaVersion = "0.0.0.0"
  , schemaTypes = []
  }

data Type = Type
  { typeName :: Identifier
  , typeDesc :: TypeDesc
  } deriving (Show)

data TypeDesc
  = Synonym Identifier
  | Range Offset Size
  | Array Identifier Size
  | Vector Identifier Size
  | Enumeration [Identifier]
  | Record [Field]
  | Combination [Field]
  | Union [Field]
  deriving (Show)

data Prim
  = PU8
  | PU16
  | PU32
  | PU64
  | PS8
  | PS16
  | PS32
  | PS64
  | PF32
  | PF64
  | PBool
  deriving (Show, Eq)

data Field
  = DataField Identifier Identifier
  | EmptyField Identifier
  deriving (Show)

type Offset = Int64
type Size = Word64
newtype Identifier = Identifier Text deriving (Show, Eq)

mkIdentifier :: String -> Maybe Identifier
mkIdentifier [] = Just (Identifier empty)
mkIdentifier i@(s:r) =
    if first && rest
      then Just (Identifier $ pack i)
      else Nothing
  where
    first = isAsciiLower s
    rest = all (\c -> isAsciiLower c || isDigit c || ('_' == c)) r

instance IsString Identifier where
  fromString s =
    fromMaybe
      (error $ "IsString Identifier: invalid input string \"" ++ s ++ "\"")
      (mkIdentifier s)

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
    A (Ident x) -> Left ("Invalid prototype: " ++ show x)
    y -> Left ("Invalid type expression: " ++ show y)
  where
    mkTD n t = (Right . TypeDef) (Type (Identifier n) t)

    toSynonym [A (Ident name), A (Ident ref)] =
      mkTD name (Synonym (Identifier ref))
    toSynonym x = Left ("Unexpected synonym body: " ++ show x)

    toRange [A (Ident name), A (Number rmin), A (Number rmax)] =
      mkTD name (Range (fromIntegral rmin) (fromIntegral (rmax - rmin)))
    toRange x = Left ("Unexpected range body: " ++ show x)

    toArray [A (Ident name), A (Ident ref), A (Number size)] =
      mkTD name (Array (Identifier ref) (fromIntegral size))
    toArray x = Left ("Unexpected array body: " ++ show x)

    toVector [A (Ident name), A (Ident ref), A (Number size)] =
      mkTD name (Vector (Identifier ref) (fromIntegral size))
    toVector x = Left ("Unexpected vector body: " ++ show x)

    toEnumeration :: [WellFormedSExpr Atom] -> Either String Component
    toEnumeration [A (Ident name), L (A (Ident "values"):vs)] = do
      let toValues (A (Ident val)) = Right (Identifier val)
          toValues x = Left ("Unexpected value: " ++ show x)
      vs' <- mapM toValues vs
      mkTD name (Enumeration vs')
    toEnumeration x = Left ("Unexpected enumeration body: " ++ show x)

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
