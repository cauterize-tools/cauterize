{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Common.Types.FixedArray where

import Cauterize.Common.Primitives
import Cauterize.Common.References

data TFixedArray t = TFixedArray Name t Integer
  deriving (Show, Ord, Eq)

instance References (TFixedArray Name) where
  referencesOf (TFixedArray _ n _) = [n]
