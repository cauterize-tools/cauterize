module Cauterize.Schema.Types where

import Cauterize.Common.BuiltIn
import Cauterize.Common.Named
import Cauterize.FormHash
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

instance Hashable Type where
  formHashWith ctx (TBuiltIn b) = ctx `hashFn` "built-in" `formHashWith` b
  formHashWith ctx (TScalar n b) = ctx `hashFn` "scalar" `hashFn` n `formHashWith` b
  formHashWith ctx (TConst n b i) = ctx `hashFn` "const" `hashFn` n `formHashWith` b `hashFn` padShowInteger i
  formHashWith ctx (TFixedArray n m i) = ctx `hashFn` "fixed" `hashFn` n `hashFn` m `hashFn` padShowInteger i
  formHashWith ctx (TBoundedArray n m i) = ctx `hashFn` "bounded" `hashFn` n `hashFn` m `hashFn` padShowInteger i
  formHashWith ctx (TStruct n _) = ctx `hashFn` "struct" `hashFn` n -- TODO: INCOMPLETE
  formHashWith ctx (TSet n _) = ctx `hashFn` "set" `hashFn` n -- TODO: INCOMPLETE
  formHashWith ctx (TEnum n _) = ctx `hashFn` "enum" `hashFn` n -- TODO: INCOMPLETE
  formHashWith ctx (TPartial n i _) = ctx `hashFn` "partial" `hashFn` n `hashFn` padShowInteger i
  formHashWith ctx (TPad n i) = ctx `hashFn` "pad" `hashFn` n `hashFn` padShowInteger i

padShowInteger :: Integer -> String
padShowInteger v = let w = 20
                       v' = abs v
                       v'' = show v'
                       num = replicate (w - length v'') '0' ++ v''
                   in if v < 0
                        then '-':num
                        else '+':num
