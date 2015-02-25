module Cauterize.Dynamic.Meta.Types
  ( MetaType(..)
  , MetaHeader(..)
  ) where

import Cauterize.Dynamic.Types
import Data.Word

data MetaHeader =
  MetaHeader { metaLength :: Integer
             , metaTag :: [Word8]
             }
  deriving (Show, Eq, Ord)

data MetaType =
  MetaType { unMetaType :: CautType }
  deriving (Show, Eq, Ord)
