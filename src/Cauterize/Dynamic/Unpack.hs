{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Dynamic.Unpack
  ( dynamicUnpack
  , dynamicUnpack'
  ) where

import Cauterize.Dynamic.Types
import Cauterize.Dynamic.Common
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Cauterize.Specification as S
import qualified Cauterize.CommonTypes as C
import qualified Data.ByteString as B

import Data.Serialize.IEEE754
import Data.Serialize.Get

dynamicUnpack :: S.Specification -> C.Identifier -> B.ByteString -> Either T.Text CautType
dynamicUnpack s n b = case flip runGet b $ dynamicUnpack' s n of
                        Left e -> Left $ T.pack e
                        Right r -> Right r

dynamicUnpack' :: S.Specification -> C.Identifier -> Get CautType
dynamicUnpack' s n =
  let m = S.specTypeMap s
  in do d <- dynamicUnpackDetails m n
        return CautType { ctName = n, ctDetails = d }

dynamicUnpackDetails :: TyMap -> C.Identifier -> Get CautDetails
dynamicUnpackDetails m n =
  case n `M.lookup` C.primMap of
    Just p -> dynamicUnpackPrim p
    Nothing ->
      let ty = n `lu` m
      in case S.typeDesc ty of
          S.Synonym { S.synonymRef = s } -> dynamicUnpackSynonym m s
          S.Range { S.rangeOffset = o, S.rangeLength = l, S.rangeTag = t } -> dynamicUnpackRange o l t
          S.Array { S.arrayRef = a, S.arrayLength = l } -> dynamicUnpackArray m a l
          S.Vector { S.vectorRef = v, S.vectorLength = l, S.vectorTag = t } -> dynamicUnpackVector m v l t
          S.Enumeration { S.enumerationValues = vs, S.enumerationTag = t } -> dynamicUnpackEnumeration vs t
          S.Record { S.recordFields = fs } -> dynamicUnpackRecord m fs
          S.Combination { S.combinationFields = fs, S.combinationTag = t } -> dynamicUnpackCombination m fs t
          S.Union { S.unionFields = fs, S.unionTag = t } -> dynamicUnpackUnion m fs t

dynamicUnpackPrim :: C.Prim -> Get CautDetails
dynamicUnpackPrim b =
  let b' =
        case b of
          C.PU8   -> liftM PDu8 getWord8
          C.PU16  -> liftM PDu16 getWord16le
          C.PU32  -> liftM PDu32 getWord32le
          C.PU64  -> liftM PDu64 getWord64le
          C.PS8   -> liftM (PDs8 . fromIntegral) getWord8
          C.PS16  -> liftM (PDs16 . fromIntegral) getWord16le
          C.PS32  -> liftM (PDs32 . fromIntegral) getWord32le
          C.PS64  -> liftM (PDs64 . fromIntegral) getWord64le
          C.PF32  -> liftM PDf32 getFloat32le
          C.PF64  -> liftM PDf64 getFloat64le
          C.PBool -> do
            w8 <- getWord8
            case w8 of
              0 -> return $ PDbool False
              1 -> return $ PDbool True
              x -> fail $ "unexpected value for boolean: " ++ show x
  in liftM CDPrim b'

dynamicUnpackSynonym :: TyMap -> C.Identifier -> Get CautDetails
dynamicUnpackSynonym m i = liftM CDSynonym (dynamicUnpackDetails m i)

dynamicUnpackRange :: C.Offset -> C.Length -> C.Tag -> Get CautDetails
dynamicUnpackRange o l t = do
  tag <- unpackTag t
  if fromIntegral tag > l
    then throw $ RangeDecodeOutOfBounds o l tag
    else return $ CDRange (tag + (fromIntegral o))

dynamicUnpackArray :: TyMap -> C.Identifier -> C.Length -> Get CautDetails
dynamicUnpackArray m r l =
  liftM CDArray $ replicateM (fromIntegral l) getter
  where
    getter = dynamicUnpackDetails m r

dynamicUnpackVector :: TyMap -> C.Identifier -> C.Length -> C.Tag -> Get CautDetails
dynamicUnpackVector m r maxLen t = do
  len <- unpackTag t
  if len > fromIntegral maxLen
    then fail $ "vector length out of bounds: " ++ show len ++ " > " ++ show maxLen
    else liftM CDVector $ replicateM (fromIntegral len) getter
  where
    getter = dynamicUnpackDetails m r

dynamicUnpackEnumeration :: [S.EnumVal] -> C.Tag -> Get CautDetails
dynamicUnpackEnumeration [] _ = error "dynamicUnpackEnumeration: enumerations must have at least one value!"
dynamicUnpackEnumeration vs t = do
  valIx <- unpackTag t
  if valIx > maxValIx
    then fail $ "enumeration tag out of bounds: " ++ show valIx ++ " > " ++ show maxValIx
    else return (CDEnumeration (S.enumValName (ixToVal valIx)))
  where
    maxValIx = S.enumValIndex (last vs)

    ixToVal :: Integer -> S.EnumVal
    ixToVal ix =
      let e = error "dynamicUnpackEnumeration: SHOULD NEVER HAPPEN. Tag not a val."
      in fromMaybe e (ix `M.lookup` ixMap)
    ixMap = M.fromList $ zip (map S.enumValIndex vs) vs

dynamicUnpackRecord :: TyMap -> [S.Field] -> Get CautDetails
dynamicUnpackRecord m fs =
  liftM (CDRecord . M.fromList) $ mapM (unpackField m) fs

dynamicUnpackCombination :: TyMap -> [S.Field] -> C.Tag -> Get CautDetails
dynamicUnpackCombination m fs t = do
  flags <- unpackTag t
  liftM (CDCombination . M.fromList) $ mapM (unpackField m) (setFields flags)
  where
    setFields flags = filter (\f -> flags `testBit` (fromIntegral . S.fieldIndex $ f)) fs

dynamicUnpackUnion :: TyMap -> [S.Field] -> C.Tag -> Get CautDetails
dynamicUnpackUnion m fs t = do
  tag <- liftM fromIntegral $ unpackTag t
  case tag `M.lookup` fm of
    Nothing -> fail $ "invalid union tag: " ++ show tag
    Just f -> do
      (n, d) <- unpackField m f
      return CDUnion { cdUnionFieldName = n, cdUnionFieldDetails = d }
  where
    fm = fieldsToIndexMap fs

unpackField :: TyMap -> S.Field -> Get (C.Identifier, FieldValue)
unpackField _ (S.EmptyField { S.fieldName = n }) = return (n, EmptyField)
unpackField m (S.DataField { S.fieldName = n, S.fieldRef = r }) =
  liftM (\d -> (n, DataField d)) (dynamicUnpackDetails m r)

unpackTag :: C.Tag -> Get Integer
unpackTag C.T1 = liftM fromIntegral getWord8
unpackTag C.T2 = liftM fromIntegral getWord16le
unpackTag C.T4 = liftM fromIntegral getWord32le
unpackTag C.T8 = liftM fromIntegral getWord64le
