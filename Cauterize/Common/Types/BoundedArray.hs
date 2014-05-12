{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Common.Types.BoundedArray where

import Cauterize.Common.Primitives
import Cauterize.Common.References

data TBoundedArray t = TBoundedArray Name t Integer
  deriving (Show, Ord, Eq)

instance References (TBoundedArray Name) where
  referencesOf (TBoundedArray _ n _) = [n]
