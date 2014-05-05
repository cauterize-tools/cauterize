module Cauterize.Specification.Types where

import Cauterize.FormHash
import Cauterize.Common.BuiltIn
import Cauterize.Common.Named

data Specification = Specification
  { name :: String
  , version :: String
  , hash :: String
  , forms :: [SpecForm]
  }
  deriving (Show)

data SpecForm = SpecForm Type
  deriving (Show)

data Type = TBuiltIn BuiltIn FormHash
          | TScalar String BuiltIn FormHash
          | TConst String BuiltIn Integer FormHash

          | TFixedArray String String Integer FormHash
          | TBoundedArray String String Integer BuiltIn FormHash

          | TStruct String [StructField] FormHash
          | TSet String BuiltIn [SetField] FormHash

          | TEnum String BuiltIn [EnumVariant] FormHash
          | TPartial String Integer BuiltIn [PartialVariant] FormHash

          | TPad String Integer FormHash
  deriving (Show)

data StructField = StructField String String
  deriving (Show)

data SetField = SetField String String Integer
  deriving (Show)

data EnumVariant = EnumVariant String (Maybe String) Integer
  deriving (Show)

data PartialVariant = PartialVariant String String FormHash
  deriving (Show)

instance CautName Type where
  cautName (TBuiltIn b _) = show b
  cautName (TScalar n _ _) = n
  cautName (TConst n _ _ _) = n
  cautName (TFixedArray n _ _ _) = n
  cautName (TBoundedArray n _ _ _ _) = n
  cautName (TStruct n _ _) = n
  cautName (TSet n _ _ _) = n
  cautName (TEnum n _ _ _) = n
  cautName (TPartial n _ _ _ _) = n
  cautName (TPad n _ _) = n
