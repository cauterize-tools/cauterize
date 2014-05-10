module Cauterize.Specification.Types where

{-
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
  pretty (Specification n v h fs) = parens $ hang psp 1 pfs
    where
      psp = text "specification" <+> (doubleQuotes . text) n <+> (doubleQuotes . text) v <+> text h
      pfs = vcat $ map pretty fs

instance Pretty SpecForm where
  pretty (SpecForm t) = pretty t

instance Pretty Type where
  pretty (TBuiltIn b h) = parens $ text "builtin" <+> (text . show) b <+> pretty h
  pretty (TScalar n r h) = parens $ text "scalar" <+> text n <+> (text . show) r <+> pretty h
  pretty (TConst n b i h) = parens $ text "const" <+> text n <+> (text . show) b <+> (text . show) i  <+> pretty h
  pretty (TFixedArray n m i h) = parens $ text "fixed" <+> text n <+> text m <+> (text . show) i <+> pretty h
  pretty (TBoundedArray n m i r h) = parens $ text "bounded" <+> text n <+> text m <+> (text . show) i <+> (text . show) r <+> pretty h
  pretty (TStruct n fs h) = parens $ hang pst 1 pfs
    where
      pst = text "struct" <+> text n <+> pretty h
      pfs = vcat $ map pretty fs
  pretty (TSet n r fs h) = parens $ hang pse 1 pfs
    where
      pse = text "set" <+> text n <+> (text . show) r <+> pretty h
      pfs = vcat $ map pretty fs
  pretty (TEnum n r vs h) = parens $ hang pen 1 pvs
    where
      pen = text "enum" <+> text n <+> (text . show) r <+> pretty h
      pvs = vcat $ map pretty vs
  pretty (TPartial n s r ps h) = parens $ hang ppa 1 pps
    where
      ppa = text "partial" <+> text n <+> (text . show) s <+> (text . show) r <+> pretty h
      pps = vcat $ map pretty ps
  pretty (TPad n l h) = parens $ text "pad" <+> text n <+> (text . show) l <+> pretty h

instance Pretty EnumVariant where
  pretty (EnumVariant n Nothing t) = parens $ text "var" <+> text n <+> (text . show) t
  pretty (EnumVariant n (Just m) t) = parens $ text "var" <+> text n <+> (text . show) t <+> text m

instance Pretty StructField where
  pretty (StructField n m) = parens $ text "field" <+> text n <+> text m

instance Pretty SetField where
  pretty (SetField n m i) = parens $ text "mem" <+> text n <+> text m <+> (text . show) i

instance Pretty PartialVariant where
  pretty (PartialVariant n m h) = parens $ text "var" <+> text n <+> text m <+> pretty h
  -}
