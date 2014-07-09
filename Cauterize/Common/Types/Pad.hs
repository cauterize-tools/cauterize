{-# LANGUAGE DeriveDataTypeable #-}
module Cauterize.Common.Types.Pad where

import Cauterize.Common.Primitives
import Cauterize.Common.References
import Data.Data

data TPad = TPad { padName :: Name, padLength :: Integer }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TPad where
  referencesOf (TPad _ _) = []
