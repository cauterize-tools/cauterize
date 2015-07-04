module Cauterize.Schema.TypesNew
  ( Schema(..)
  , Type(..)
  , TypeDesc(..)
  , Prim(..)
  , Field(..)
  , Offset
  , Length
  , Identifier
    , unIdentifier
    , unsafeMkIdentifier
    , mkIdentifier
  , IsSchema(..)
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
  = Synonym { synonymRef :: Identifier }
  | Range { rangeOffset :: Offset, rangeLength :: Length }
  | Array { arrayRef :: Identifier, arrayLength :: Length }
  | Vector { vectorRef :: Identifier, vectorLength :: Length }
  | Enumeration { enumerationValues :: [Identifier] }
  | Record { recordFields :: [Field] }
  | Combination { combinationFields :: [Field] }
  | Union { unionFields :: [Field] }
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
  deriving (Show, Eq, Enum, Bounded)

data Field
  = DataField { fieldName :: Identifier, fieldRef :: Identifier }
  | EmptyField { fieldName :: Identifier }
  deriving (Show)

type Offset = Int64
type Length = Word64
newtype Identifier = Identifier { unIdentifier :: Text }
  deriving (Eq, Ord)

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

instance Show Identifier where
  show (Identifier i) = "i" ++ show i

class IsSchema a where
  getSchema :: a -> Schema

instance IsSchema Schema where
  getSchema = id
