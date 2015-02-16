module Cauterize.Dynamic.Unpack
  ( dynamicUnpack
  ) where

import Cauterize.Dynamic.Types
import qualified Cauterize.Specification as S
import qualified Data.ByteString as B
import qualified Data.Text as T

dynamicUnpack :: S.Spec -> CautType -> Either T.Text B.ByteString
dynamicUnpack = undefined
