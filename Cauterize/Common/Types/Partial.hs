{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Cauterize.Common.Types.Partial where

import Cauterize.Common.Primitives
import Cauterize.Common.IndexedRef
import Cauterize.Common.References

import Data.List
import Data.Data

data TPartial t = TPartial { partialName :: Name, partialFields :: Fields t }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References (TPartial Name) where
  referencesOf (TPartial _ (Fields rs)) = nub $ map refRef rs
