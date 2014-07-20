{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}

module Cauterize.Common.Types where

import Cauterize.Common.Primitives
import Cauterize.Common.References
import Cauterize.Common.IndexedRef
import Data.Data
import Data.List

data TBuiltIn = TBuiltIn { unTBuiltIn :: BuiltIn }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TBuiltIn where
  referencesOf _ = []


data TConst = TConst { constName :: Name
                     , constRepr :: BuiltIn
                     , constValue :: Integer }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TConst where
  referencesOf (TConst _ b _) = [show b]


data TFixedArray t = TFixedArray { fixedArrName :: Name,
                                   fixedArrRef :: t,
                                   fixedArrLen :: Integer }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References (TFixedArray Name) where
  referencesOf (TFixedArray _ n _) = [n]


data TBoundedArray t = TBoundedArray { boundedArrName :: Name
                                     , boundedArrRef :: t
                                     , boundedArrMaxLen :: Integer
                                     }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References (TBoundedArray Name) where
  referencesOf (TBoundedArray _ n _) = [n]


data TScalar = TScalar { scalarName :: Name
                       , scalarRepr :: BuiltIn }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TScalar where
  referencesOf (TScalar _ b) = [show b]


data TStruct t = TStruct { structName :: Name, structFields :: Fields t }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References (TStruct Name) where
  referencesOf (TStruct _ (Fields rs)) = nub $ map refRef rs


data TEnum t = TEnum { enumName :: Name, enumFields :: Fields t }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References (TEnum Name) where
  referencesOf (TEnum _ (Fields rs)) = nub $ map refRef rs


data TSet t = TSet { setName :: Name, setFields :: Fields t }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References (TSet Name) where
  referencesOf (TSet _ (Fields rs)) = nub $ map refRef rs


data TPartial t = TPartial { partialName :: Name, partialFields :: Fields t }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References (TPartial Name) where
  referencesOf (TPartial _ (Fields rs)) = nub $ map refRef rs


data TPad = TPad { padName :: Name, padLength :: Integer }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TPad where
  referencesOf (TPad _ _) = []


