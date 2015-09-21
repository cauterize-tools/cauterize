{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
module Cauterize.Schema.Parser
  ( parseSchema
  , parseSchemaFromFile
  , formatSchema
  ) where

import Control.Monad
import Data.SCargot.General
import Data.SCargot.Repr
import Data.SCargot.Repr.WellFormed
import Data.SCargot.Pretty
import Data.SCargot.Comments
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Parsec
import Text.Parsec.Text

import Cauterize.Schema.Types
import Cauterize.CommonTypes

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
sAtom (Str v) = T.concat ["\"", v, "\""]

toComponent :: WellFormedSExpr Atom -> Either String Component
toComponent = asList go
  where
    go [A (Ident "name"), A (Str name)] = Right (Name name)
    go [A (Ident "version"), A (Str version)] = Right (Version version)
    go (A (Ident "type") : rs ) = toType rs
    go (A (Ident x) : _ ) = Left ("Unhandled component: " ++ show x)
    go y = Left ("Not a component name: " ++ show y)

pattern AI x = A (Ident x)
pattern AN x = A (Number x)

toType :: [WellFormedSExpr Atom] -> Either String Component
toType [] = Left "Empty type expression."
toType (tproto:tbody) =
  case tproto of
    AI "synonym" -> toSynonym tbody
    AI "range" -> toRange tbody
    AI "array" -> toArray tbody
    AI "vector" -> toVector tbody
    AI "enumeration" -> toEnumeration tbody
    AI "record" -> toRecord tbody
    AI "combination" -> toCombination tbody
    AI "union" -> toUnion tbody
    AI x -> Left ("Invalid prototype: " ++ show x)
    y -> Left ("Invalid type expression: " ++ show y)
  where
    umi = unsafeMkIdentifier . unpack
    mkTD n t = (Right . TypeDef) (Type (umi n) t)

    toField (L [AI "field", AI name, AI ref]) =
      Right $ DataField (umi name) (umi ref)
    toField (L [AI "empty", AI name]) =
      Right $ EmptyField (umi name)
    toField x = Left ("Unexpected field body: " ++ show x)

    toSynonym [AI name, AI ref] =
      mkTD name (Synonym (umi ref))
    toSynonym x = Left ("Unexpected synonym body: " ++ show x)

    toRange [AI name, AN rmin, AN rmax] =
      mkTD name (Range (fromIntegral rmin) (fromIntegral (rmax - rmin)))
    toRange x = Left ("Unexpected range body: " ++ show x)

    toArray [AI name, AI ref, AN size]
      | size < 0 = Left ("Size must be positive: " ++ show size)
      | otherwise = mkTD name (Array (umi ref) (fromIntegral size))
    toArray x = Left ("Unexpected array body: " ++ show x)

    toVector [AI name, AI ref, AN size]
      | size < 0 = Left ("Size must be positive: " ++ show size)
      | otherwise = mkTD name (Vector (umi ref) (fromIntegral size))
    toVector x = Left ("Unexpected vector body: " ++ show x)

    toEnumeration [AI name, L (AI "values":vs)] = do
      let toValue (AI val) = Right (umi val)
          toValue x = Left ("Unexpected value: " ++ show x)
      vs' <- mapM toValue vs
      mkTD name (Enumeration vs')
    toEnumeration x = Left ("Unexpected enumeration body: " ++ show x)

    toRecord [AI name, L (AI "fields":fs)] = do
      fs' <- mapM toField fs
      mkTD name (Record fs')
    toRecord x = Left ("Unexpected record body: " ++ show x)

    toCombination [AI name, L (AI "fields":fs)] = do
      fs' <- mapM toField fs
      mkTD name (Combination fs')
    toCombination x = Left ("Unexpected combination body: " ++ show x)

    toUnion [AI name, L (AI "fields":fs)] = do
      fs' <- mapM toField fs
      mkTD name (Union fs')
    toUnion x = Left ("Unexpected union body: " ++ show x)

fromType :: Type -> WellFormedSExpr Atom
fromType (Type n d) = L (A (Ident "type") : rest)
  where
    na = A (Ident (unIdentifier n))
    ai = A . Ident
    aiu = A . Ident . unIdentifier
    an = A . Number

    fromField (DataField fn fr) = L [ai "field", aiu fn, aiu fr]
    fromField (EmptyField fn) = L [ai "empty", aiu fn]

    rest =
      case d of
        Synonym { synonymRef = r } ->
          [ai "synonym", na, aiu r]
        Range { rangeOffset = o, rangeLength = l } ->
          [ai "range", na, an (fromIntegral o), an (fromIntegral o + fromIntegral l)]
        Array { arrayRef = r, arrayLength = l } ->
          [ai "array", na, aiu r, an (fromIntegral l)]
        Vector { vectorRef = r, vectorLength = l } ->
          [ai "vector", na, aiu r, an (fromIntegral l)]
        Enumeration { enumerationValues = vs } ->
          [ai "enumeration", na, L (ai "values" : map aiu vs)]
        Record { recordFields = fs } ->
          [ai "record", na, L (ai "fields" : map fromField fs)]
        Combination { combinationFields = fs } ->
          [ai "combination", na, L (ai "fields" : map fromField fs)]
        Union { unionFields = fs } ->
          [ai "union", na, L (ai "fields" : map fromField fs)]

fromComponent :: Component -> WellFormedSExpr Atom
fromComponent c =
  case c of
    (Name n) -> L [ ident "name", A . Str $ n ]
    (Version v) -> L [ ident "version", A . Str $ v ]
    (TypeDef t) -> fromType t
  where
    ident = A . Ident . pack

cauterizeSpec :: SExprSpec Atom Component
cauterizeSpec = convertSpec toComponent fromComponent $ withLispComments $ asWellFormed $ mkSpec pAtom sAtom

componentsToSchema :: [Component] -> Schema
componentsToSchema = foldl go defaultSchema
  where
    go s (Name n) = s { schemaName = n }
    go s (Version v) = s { schemaVersion = v }
    go s (TypeDef t) = let ts = schemaTypes s
                        in s { schemaTypes = t:ts }

schemaToComponents :: Schema -> [Component]
schemaToComponents s =
    Name (schemaName s)
  : Version (schemaVersion s)
  : map TypeDef (schemaTypes s)

parseSchema :: Text -> Either String Schema
parseSchema t = componentsToSchema `fmap` decode cauterizeSpec t

formatSchema :: IsSchema a => a -> Text
formatSchema s = let pp = prettyPrintSExpr (basicPrint sAtom)
                     s' = map (pp . fromWellFormed . fromComponent) (schemaToComponents (getSchema s))
                 in T.unlines s'

parseSchemaFromFile :: FilePath -> IO (Either String Schema)
parseSchemaFromFile p = liftM parseSchema (T.readFile p)
