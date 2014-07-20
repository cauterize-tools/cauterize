{-# LANGUAGE DeriveDataTypeable #-}
module Cauterize.Common.Types.Scalar where

import Cauterize.Common.Primitives
import Cauterize.Common.References
import Data.Data

data TScalar = TScalar { scalarName :: Name
                       , scalarRepr :: BuiltIn }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TScalar where
  referencesOf (TScalar _ b) = [show b]
