{-# LANGUAGE DeriveDataTypeable #-}

module Cauterize.Common.Types.BuiltIn where

import Cauterize.Common.Primitives
import Cauterize.Common.References
import Data.Data

data TBuiltIn = TBuiltIn { unTBuiltIn :: BuiltIn }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TBuiltIn where
  referencesOf _ = []
