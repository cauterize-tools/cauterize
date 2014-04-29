module Cauterize.Schema.Types where

import Cauterize.Common.BuiltIn

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
          | TSet String [SetField]

          | TEnum String [EnumVariant]
          | TPartial String Integer [PartialVariant]

          | TPad String Integer
  deriving (Show)

data StructField = StructField String String
  deriving (Show)

data EnumVariant = EnumVariant String (Maybe String)
  deriving (Show)

data PartialVariant = PartialVariant String String
  deriving (Show)

data SetField = SetField String String
  deriving (Show)

