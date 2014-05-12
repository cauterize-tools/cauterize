module Cauterize.Common.Types.Pad where

import Cauterize.Common.Primitives
import Cauterize.Common.References

data TPad = TPad Name Integer
  deriving (Show, Ord, Eq)

instance References TPad where
  referencesOf (TPad _ _) = []
