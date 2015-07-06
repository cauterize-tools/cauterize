{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Dynamic.Common
  ( isNameOf
  , lu
  , fieldsToNameMap
  , fieldsToIndexMap
  , fieldNameSet

  , isSynonym
  , isRange
  , isArray
  , isVector
  , isEnumeration
  , isRecord
  , isCombination
  , isUnion
  ) where

import Cauterize.Dynamic.Types
import Control.Exception
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Cauterize.Specification as S
import qualified Cauterize.CommonTypes as C
import qualified Data.Map as M
import qualified Data.Set as Set

lu :: C.Identifier -> TyMap -> S.Type
lu n m = fromMaybe (throw $ InvalidType n)
                   (n `M.lookup` m)

fieldsToNameMap :: [S.Field] -> M.Map C.Identifier S.Field
fieldsToNameMap fs = M.fromList $ map go fs
  where
    go f = (S.fieldName f, f)

fieldsToIndexMap :: [S.Field] -> M.Map Integer S.Field
fieldsToIndexMap fs = M.fromList $ map go fs
  where
    go f = (S.fieldIndex f, f)

fieldNameSet :: [S.Field] -> Set.Set C.Identifier
fieldNameSet fs = Set.fromList $ map S.fieldName fs

isNameOf :: T.Text -> PrimDetails -> Bool
isNameOf "u8"   (PDu8 _) = True
isNameOf "u16"  (PDu16 _) = True
isNameOf "u32"  (PDu32 _) = True
isNameOf "u64"  (PDu64 _) = True
isNameOf "s8"   (PDs8 _) = True
isNameOf "s16"  (PDs16 _) = True
isNameOf "s32"  (PDs32 _) = True
isNameOf "s64"  (PDs64 _) = True
isNameOf "f32"  (PDf32 _) = True
isNameOf "f64"  (PDf64 _) = True
isNameOf "bool" (PDbool _) = True
isNameOf _ _ = False

isSynonym :: S.Type -> Bool
isSynonym (S.Type _ _ _ (S.Synonym {})) = True
isSynonym _ = False

isRange :: S.Type -> Bool
isRange (S.Type _ _ _ (S.Range {})) = True
isRange _ = False

isArray :: S.Type -> Bool
isArray (S.Type _ _ _ (S.Array {})) = True
isArray _ = False

isVector :: S.Type -> Bool
isVector (S.Type _ _ _ (S.Vector {})) = True
isVector _ = False

isEnumeration :: S.Type -> Bool
isEnumeration (S.Type _ _ _ (S.Enumeration {})) = True
isEnumeration _ = False

isRecord :: S.Type -> Bool
isRecord (S.Type _ _ _ (S.Record {})) = True
isRecord _ = False

isCombination :: S.Type -> Bool
isCombination (S.Type _ _ _ (S.Combination {})) = True
isCombination _ = False

isUnion :: S.Type -> Bool
isUnion (S.Type _ _ _ (S.Union {})) = True
isUnion _ = False
