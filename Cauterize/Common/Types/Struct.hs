{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Common.Types.Struct where

import Cauterize.Common.Primitives
import Cauterize.Common.IndexedRef
import Cauterize.Common.References

import Data.List

data TStruct t = TStruct Name [IndexedRef t]
  deriving (Show, Ord, Eq)

instance References (TStruct Name) where
  referencesOf (TStruct _ rs) = nub $ map refRef rs
