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

data Type = TBuiltIn BuiltIn
          | TScalar String BuiltIn
          | TConst String BuiltIn Integer

          | TFixedArray String String Integer
          | TBoundedArray String String Integer BuiltIn

          | TStruct String [StructField]
          | TSet String BuiltIn [SetField]

          | TEnum String BuiltIn [EnumVariant]
          | TPartial String Integer BuiltIn [PartialVariant]

          | TPad String Integer
  deriving (Show)

data StructField = StructField String String
  deriving (Show)

data SetField = SetField String String Integer
  deriving (Show)

data EnumVariant = EnumVariant String (Maybe String) Integer
  deriving (Show)

data PartialVariant = PartialVariant String String FormHash
  deriving (Show)

instance Hashable Specification where
  formHashWith ctx (Specification n v _ fs) = finalCtx
    where
      nextCtx = foldl hashFn ctx ["specification", n, v]
      finalCtx = foldl formHashWith nextCtx fs

instance Hashable SpecForm where
  formHashWith ctx _ = ctx `hashFn` "specform"

instance CautName Type where
  cautName (TBuiltIn b) = show b
  cautName (TScalar n _) = n
  cautName (TConst n _ _) = n
  cautName (TFixedArray n _ _) = n
  cautName (TBoundedArray n _ _ _) = n
  cautName (TStruct n _) = n
  cautName (TSet n _ _) = n
  cautName (TEnum n _ _) = n
  cautName (TPartial n _ _ _) = n
  cautName (TPad n _) = n
