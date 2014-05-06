module Cauterize.Specification.Types where

import Cauterize.FormHash
import Cauterize.Common.BuiltIn
import Cauterize.Common.Named

import Text.PrettyPrint
import Text.PrettyPrint.Class

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

instance Pretty Specification where
  pretty (Specification n v h fs) = parens $ text "specification" <+> (doubleQuotes . text) n <+> (doubleQuotes . text) v <+> text h <+> pfs
    where
      pfs = vcat $ map pretty fs

instance Pretty SpecForm where
  pretty (SpecForm t) = pretty t

instance Pretty Type where
  pretty (TBuiltIn b h) = parens $ text "builtin" <+> (text . show) b <+> pretty h
  pretty (TScalar n r h) = parens $ text "scalar" <+> text n <+> (text . show) r <+> pretty h
  pretty (TConst n b i h) = parens $ text "const" <+> text n <+> (text . show) b
  pretty (TFixedArray n m i h) = parens $ text "fixed" <+> text n
  pretty (TBoundedArray n m i r h) = parens $ text "bounded" <+> text n
  pretty (TStruct n fs h) = parens $ text "struct" <+> text n
  pretty (TSet n r fs h) = parens $ text "set" <+> text n
  pretty (TEnum n r vs h) = parens $ text "enum" <+> text n <+> (text . show) r <+> pretty h <+> pvs
    where
      pvs = vcat $ map pretty vs
  pretty (TPartial n s r ps h) = parens $ text "partial" <+> text n
  pretty (TPad n l h) = parens $ text "pad" <+> text n

instance Pretty EnumVariant where
  pretty (EnumVariant n Nothing t) = parens $ text "var" <+> text n <+> (text . show) t
  pretty (EnumVariant n (Just m) t) = parens $ text "var" <+> text n <+> (text . show) t <+> text m
