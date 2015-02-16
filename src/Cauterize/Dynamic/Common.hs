module Cauterize.Dynamic.Common
  ( isNameOf
  , lu
  , throwTM

  , isBuiltIn
  ) where

import Cauterize.Dynamic.Types
import Control.Exception
import Data.Maybe
import qualified Cauterize.Specification as S
import qualified Data.Map as M

isNameOf :: String -> BIDetails -> Bool
isNameOf "u8" (BDu8 _) = True
isNameOf "u16" (BDu16 _) = True
isNameOf "u32" (BDu32 _) = True
isNameOf "u64" (BDu64 _) = True
isNameOf "s8" (BDs8 _) = True
isNameOf "s16" (BDs16 _) = True
isNameOf "s32" (BDs32 _) = True
isNameOf "s64" (BDs64 _) = True
isNameOf "f32" (BDf32 _) = True
isNameOf "f64" (BDf64 _) = True
isNameOf "bool" (BDbool _) = True
isNameOf "cu8" (BDcu8 _) = True
isNameOf "cu16" (BDcu16 _) = True
isNameOf "cu32" (BDcu32 _) = True
isNameOf _ _ = False

lu :: String -> TyMap -> S.SpType
lu n m = fromMaybe (error $ "ERROR: '" ++ n ++ "' is not a valid type in the provided map.")
                   (n `M.lookup` m)


isBuiltIn :: S.SpType -> Bool
isBuiltIn (S.BuiltIn {}) = True
isBuiltIn _ = False

throwTM :: String -> c
throwTM = throw . TypeMisMatch
