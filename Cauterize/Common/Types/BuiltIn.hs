module Cauterize.Common.Types.BuiltIn where

import Cauterize.Common.Primitives
import Cauterize.Common.References

data TBuiltIn = TBuiltIn BuiltIn
  deriving (Show, Ord, Eq)

instance References TBuiltIn where
  referencesOf _ = []
