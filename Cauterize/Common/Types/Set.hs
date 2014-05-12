{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Common.Types.Set where

import Cauterize.Common.Primitives
import Cauterize.Common.IndexedRef
import Cauterize.Common.References

import Data.List

data TSet t = TSet Name [IndexedRef t]
  deriving (Show, Ord, Eq)

instance References (TSet Name) where
  referencesOf (TSet _ rs) = nub $ map refRef rs
