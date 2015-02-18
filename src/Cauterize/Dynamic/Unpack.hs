module Cauterize.Dynamic.Unpack
  ( dynamicUnpack
  ) where

import Cauterize.Dynamic.Types
import qualified Cauterize.Specification as S
import qualified Data.ByteString as B

import Data.Serialize.IEEE754
import Data.Serialize.Get

dynamicUnpack :: S.Spec -> B.ByteString -> String -> Either String CautType
dynamicUnpack s b n =
  let m = S.specTypeMap s
  in runGet (dynamicUnpackDetails m n) b

dynamicUnpackDetails :: TyMap -> String -> Get CautType
dynamicUnpackDetails m n = undefined
