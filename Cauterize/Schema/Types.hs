module Cauterize.Schema.Types where

import Cauterize.Common.BuiltIn
import Cauterize.Common.Named
import Data.List
import Data.Maybe

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

instance CautName Type where
  cautName (TBuiltIn b) = show b
  cautName (TScalar n _) = n
  cautName (TConst n _ _) = n
  cautName (TFixedArray n _ _) = n
  cautName (TBoundedArray n _ _) = n
  cautName (TStruct n _) = n
  cautName (TSet n _) = n
  cautName (TEnum n _) = n
  cautName (TPartial n _ _) = n
  cautName (TPad n _) = n

instance RefersNames Type where
  referredNames (TBuiltIn _) = []
  referredNames (TScalar _ b) = [cautName b]
  referredNames (TConst _ b _) = [cautName b]
  referredNames (TFixedArray _ n _) = [n]
  referredNames (TBoundedArray _ n _) = [n]
  referredNames (TStruct _ fs) = nub $  map (\(StructField _ n) -> n) fs
  referredNames (TSet _ fs) = nub $  map (\(SetField _ n) -> n) fs
  referredNames (TEnum _ vs) = nub $ mapMaybe (\(EnumVariant _ n) -> n) vs
  referredNames (TPartial _ _ fs) = nub $ map (\(PartialVariant _ n) -> n) fs
  referredNames (TPad _ _) = []
