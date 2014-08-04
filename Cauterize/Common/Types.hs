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


data TFixedArray = TFixedArray { fixedArrName :: Name
                               , fixedArrRef :: Name
                               , fixedArrLen :: Integer
                               }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TFixedArray where
  referencesOf (TFixedArray _ n _) = [n]


data TBoundedArray = TBoundedArray { boundedArrName :: Name
                                   , boundedArrRef :: Name
                                   , boundedArrMaxLen :: Integer
                                   }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TBoundedArray where
  referencesOf (TBoundedArray _ n _) = [n]


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


