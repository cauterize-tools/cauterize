module Cauterize.Common.References where

import Cauterize.Common.Primitives

class References a where
  referencesOf :: a -> [Name]
