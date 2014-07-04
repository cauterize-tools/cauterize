{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Cauterize.Common.Types.BoundedArray where

import Cauterize.Common.Primitives
import Cauterize.Common.References
import Data.Data

data TBoundedArray t = TBoundedArray { boundedArrName :: Name
                                     , boundedArrRef :: t
                                     , boundedArrMaxSize :: Integer
                                     }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References (TBoundedArray Name) where
  referencesOf (TBoundedArray _ n _) = [n]
