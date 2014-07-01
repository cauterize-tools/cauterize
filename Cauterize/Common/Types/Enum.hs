{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Cauterize.Common.Types.Enum where

import Cauterize.Common.Primitives
import Cauterize.Common.IndexedRef
import Cauterize.Common.References

import Data.List
import Data.Data

data TEnum t = TEnum Name (Fields t)
  deriving (Show, Ord, Eq, Data, Typeable)

instance References (TEnum Name) where
  referencesOf (TEnum _ (Fields rs)) = nub $ map refRef rs
