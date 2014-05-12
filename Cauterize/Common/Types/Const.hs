module Cauterize.Common.Types.Const where

import Cauterize.Common.Primitives
import Cauterize.Common.References

data TConst = TConst Name BuiltIn Integer
  deriving (Show, Ord, Eq)

instance References TConst where
  referencesOf (TConst _ b _) = [show b]
