{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Cauterize.Common.Types.FixedArray where

import Cauterize.Common.Primitives
import Cauterize.Common.References
import Data.Data

data TFixedArray t = TFixedArray { fixedArrName :: Name,
                                   fixedArrRef :: t,
                                   fixedArrLen :: Integer }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References (TFixedArray Name) where
  referencesOf (TFixedArray _ n _) = [n]
