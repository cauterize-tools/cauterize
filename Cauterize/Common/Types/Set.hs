{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Cauterize.Common.Types.Set where

import Cauterize.Common.Primitives
import Cauterize.Common.IndexedRef
import Cauterize.Common.References

import Data.List
import Data.Data

data TSet t = TSet { setName :: Name, setFields :: Fields t }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References (TSet Name) where
  referencesOf (TSet _ (Fields rs)) = nub $ map refRef rs
