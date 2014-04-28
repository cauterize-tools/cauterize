module Cauterize.Schema.Types where

data Schema = Schema
  { schemaName :: String
  , schemaVersion :: String
  , schemaForms :: [SchemaForm]
  }
  deriving (Show)

data SchemaForm = FType Type
  deriving (Show)

{- 
 - Valid types in Cauterize
 -
 -    * BuiltIn
 -    * Enumeration
 -    * BoundedArray
 -    * UnboundedArray
 -    * Set
 -    * Constant
 -}
data Type = TBuiltIn BuiltIn
          | TScalar String BuiltIn
          | TConst String BuiltIn Integer
          | TFixedArray String String Integer
          | TBoundedArray String String Integer
          | TStruct String [StructField]
          | TEnum String [EnumVariant]
          | TSet String [SetField]
          | TPad String Integer
  deriving (Show)

data BuiltIn = BIu8 | BIu16 | BIu32 | BIu64
             | BIs8 | BIs16 | BIs32 | BIs64
             | BIieee754s | BIieee754d
             | BIbool
  deriving (Enum, Bounded)

data StructField = StructField String String
  deriving (Show)

data EnumVariant = EnumVariant String (Maybe String)
  deriving (Show)

data SetField = SetField String String
  deriving (Show)

instance Show BuiltIn where
  show BIu8       = "u8"
  show BIu16      = "u16"
  show BIu32      = "u32"
  show BIu64      = "u64"
  show BIs8       = "s8"
  show BIs16      = "s16"
  show BIs32      = "s32"
  show BIs64      = "s64"
  show BIbool     = "bool"
  show BIieee754s = "ieee754s"
  show BIieee754d = "ieee754d"

instance Read BuiltIn where
  readsPrec _ "u8"       = [ (BIu8, "") ]
  readsPrec _ "u16"      = [ (BIu16, "") ]
  readsPrec _ "u32"      = [ (BIu32, "") ]
  readsPrec _ "u64"      = [ (BIu64, "") ]
  readsPrec _ "s8"       = [ (BIs8, "") ]
  readsPrec _ "s16"      = [ (BIs16, "") ]
  readsPrec _ "s32"      = [ (BIs32, "") ]
  readsPrec _ "s64"      = [ (BIs64, "") ]
  readsPrec _ "bool"     = [ (BIbool, "") ]
  readsPrec _ "ieee754s" = [ (BIieee754s, "") ]
  readsPrec _ "ieee754d" = [ (BIieee754d, "") ]
  readsPrec _ s = error $ "ERROR: \"" ++ s ++ "\" is not a BuiltIn."

