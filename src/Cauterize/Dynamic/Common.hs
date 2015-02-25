module Cauterize.Dynamic.Common
  ( isNameOf
  , lu
  , fieldsToNameMap
  , fieldsToIndexMap
  , fieldNameSet

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
import qualified Cauterize.Common.Types as C
import qualified Data.Map as M
import qualified Data.Set as Set

lu :: String -> TyMap -> S.SpType
lu n m = fromMaybe (throw $ InvalidType n)
                   (n `M.lookup` m)

fieldsToNameMap :: [C.Field] -> M.Map String C.Field
fieldsToNameMap fs = M.fromList $ map go fs
  where
    go f = (C.fName f, f)

fieldsToIndexMap :: [C.Field] -> M.Map Integer C.Field
fieldsToIndexMap fs = M.fromList $ map go fs
  where
    go f = (C.fIndex f, f)

fieldNameSet :: [C.Field] -> Set.Set String
fieldNameSet fs = Set.fromList $ map C.fName fs

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
