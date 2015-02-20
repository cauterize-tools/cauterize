module Cauterize.Dynamic.Meta.Types
  ( MetaType(..)
  ) where

import Cauterize.Dynamic.Types

data MetaType =
  MetaType { unMetaType :: CautType }
