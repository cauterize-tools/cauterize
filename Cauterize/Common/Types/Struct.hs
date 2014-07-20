{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Cauterize.Common.Types.Struct where

import Cauterize.Common.Primitives
import Cauterize.Common.IndexedRef
import Cauterize.Common.References

import Data.List
import Data.Data

data TStruct t = TStruct { structName :: Name, structFields :: Fields t }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References (TStruct Name) where
  referencesOf (TStruct _ (Fields rs)) = nub $ map refRef rs
