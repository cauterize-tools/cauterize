module Cauterize.Common.Types.Scalar where

import Cauterize.Common.Primitives
import Cauterize.Common.References

data TScalar = TScalar Name BuiltIn
  deriving (Show, Ord, Eq)

instance References TScalar where
  referencesOf (TScalar _  b) = [show b]
