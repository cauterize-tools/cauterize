module Cauterize.Dynamic.Common
  ( isNameOf
  , lu

  , throwTM
  , throwIAL
  , throwIT

  , isBuiltIn
  , isSynonym
  , isArray
  , isVector
  , isRecord
  , isCombination
  , isUnion
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

isBuiltIn :: S.SpType -> Bool
isBuiltIn (S.BuiltIn {}) = True
isBuiltIn _ = False

isSynonym :: S.SpType -> Bool
isSynonym (S.Synonym {}) = True
isSynonym _ = False

isArray :: S.SpType -> Bool
isArray (S.Array {}) = True
isArray _ = False

isVector :: S.SpType -> Bool
isVector (S.Vector {}) = True
isVector _ = False

isRecord :: S.SpType -> Bool
isRecord (S.Record {}) = True
isRecord _ = False

isCombination :: S.SpType -> Bool
isCombination (S.Combination {}) = True
isCombination _ = False

isUnion :: S.SpType -> Bool
isUnion (S.Union {}) = True
isUnion _ = False

lu :: String -> TyMap -> S.SpType
lu n m = fromMaybe (throwIT $ "'" ++ n ++ "' is not a valid type in the provided map.")
                   (n `M.lookup` m)

throwTM :: String -> c
throwTM = throw . TypeMisMatch

throwIAL :: String -> c
throwIAL = throw . IncorrectArrayLength

throwIT :: String -> c
throwIT = throw . InvalidType
