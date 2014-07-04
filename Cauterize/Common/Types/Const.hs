{-# LANGUAGE DeriveDataTypeable #-}
module Cauterize.Common.Types.Const where

import Cauterize.Common.Primitives
import Cauterize.Common.References
import Data.Data

data TConst = TConst { constName :: Name
                     , constRepr :: BuiltIn
                     , constValue :: Integer }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TConst where
  referencesOf (TConst _ b _) = [show b]
