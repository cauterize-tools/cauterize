{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}

module Cauterize.Common.Types where

import Cauterize.Common.Primitives
import Cauterize.Common.References
import Cauterize.Common.Field
import Data.Data
import Data.List
import Data.Maybe

data TBuiltIn = TBuiltIn { unTBuiltIn :: BuiltIn }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TBuiltIn where
  referencesOf _ = []


data TConst = TConst { constName :: Name
                     , constRepr :: BuiltIn
                     , constValue :: Integer
                     }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TConst where
  referencesOf (TConst _ b _) = [show b]


data TArray = TArray { arrayName :: Name
                     , arrayRef :: Name
                     , arrayLen :: Integer
                     }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TArray where
  referencesOf (TArray _ n _) = [n]


data TVector = TVector { vectorName :: Name
                       , vectorRef :: Name
                       , vectorMaxLen :: Integer
                       }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TVector where
  referencesOf (TVector _ n _) = [n]


data TScalar = TScalar { scalarName :: Name
                       , scalarRepr :: BuiltIn }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TScalar where
  referencesOf (TScalar _ b) = [show b]


data TStruct = TStruct { structName :: Name, structFields :: Fields }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TStruct where
  referencesOf (TStruct _ (Fields rs)) = nub $ mapMaybe refRef rs


data TEnum = TEnum { enumName :: Name, enumFields :: Fields }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TEnum where
  referencesOf (TEnum _ (Fields rs)) = nub $ mapMaybe refRef rs


data TSet = TSet { setName :: Name, setFields :: Fields }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TSet where
  referencesOf (TSet _ (Fields rs)) = nub $ mapMaybe refRef rs


data TPad = TPad { padName :: Name, padLength :: Integer }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TPad where
  referencesOf (TPad _ _) = []


