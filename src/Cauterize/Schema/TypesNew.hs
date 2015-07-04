module Cauterize.Schema.TypesNew
  ( Schema(..)
  , Type(..)
  , TypeDesc(..)
  , Prim(..)
  , Field(..)
  , Offset
  , Size
  , Identifier, unsafeMkIdentifier, mkIdentifier
  ) where

-- TODO: Add a safe constructor for Text -> Identifier

import Data.Char
import Data.Int
import Data.Maybe
import Data.String
import Data.Text (Text, pack, empty)
import Data.Word

data Schema = Schema
  { schemaName :: Text
  , schemaVersion :: Text
  , schemaTypes :: [Type]
  } deriving (Show)

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

isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (s:r) = first && rest
  where
    first = isAsciiLower s
    rest = all (\c -> isAsciiLower c || isDigit c || ('_' == c)) r

unsafeMkIdentifier :: String -> Identifier
unsafeMkIdentifier s =
  fromMaybe
    (error $ "unsafeMkIdentifier: invalid input string \"" ++ s ++ "\"")
    (mkIdentifier s)

mkIdentifier :: String -> Maybe Identifier
mkIdentifier [] = Just (Identifier empty)
mkIdentifier i =
    if isValidIdentifier i
      then Just (Identifier $ pack i)
      else Nothing

instance IsString Identifier where
  fromString s =
    fromMaybe
      (error $ "IsString Identifier: invalid input string \"" ++ s ++ "\"")
      (mkIdentifier s)
