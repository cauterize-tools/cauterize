module Cauterize.Dynamic.Unpack
  ( dynamicUnpack
  , dynamicUnpack'
  ) where

import Cauterize.Dynamic.Types
import Cauterize.Dynamic.Common
import Control.Exception
import Control.Monad
import Data.Bits
import qualified Data.Map as M
import qualified Cauterize.Specification as S
import qualified Cauterize.Common.Types as C
import qualified Data.ByteString as B

import Data.Serialize.IEEE754
import Data.Serialize.Get

dynamicUnpack :: S.Spec -> String -> B.ByteString -> Either String CautType
dynamicUnpack s n b = flip runGet b $ dynamicUnpack' s n

dynamicUnpack' :: S.Spec -> String -> Get CautType
dynamicUnpack' s n =
  let m = S.specTypeMap s
  in do d <- dynamicUnpackDetails m n
        return CautType { ctName = n, ctDetails = d }

dynamicUnpackDetails :: TyMap -> String -> Get CautDetails
dynamicUnpackDetails m n =
  let t = n `lu` m
  in case t of
      S.BuiltIn { S.unBuiltIn = b } -> dynamicUnpackBuiltIn m b
      S.Synonym { S.unSynonym = s } -> dynamicUnpackSynonym m s
      S.Array { S.unArray = a } -> dynamicUnpackArray m a
      S.Vector { S.unVector = v, S.lenRepr = lr } -> dynamicUnpackVector m v lr
      S.Record { S.unRecord = r } -> dynamicUnpackRecord m r
      S.Combination { S.unCombination = c, S.flagsRepr = fr } -> dynamicUnpackCombination m c fr
      S.Union { S.unUnion = u, S.tagRepr = tr } -> dynamicUnpackUnion m u tr

dynamicUnpackBuiltIn :: TyMap -> C.TBuiltIn -> Get CautDetails
dynamicUnpackBuiltIn _ (C.TBuiltIn b) = liftM CDBuiltIn (unpackBuiltIn b)

dynamicUnpackSynonym :: TyMap -> C.TSynonym -> Get CautDetails
dynamicUnpackSynonym _ (C.TSynonym { C.synonymRepr = r }) = liftM CDSynonym (unpackBuiltIn r)

dynamicUnpackArray :: TyMap -> C.TArray -> Get CautDetails
dynamicUnpackArray m (C.TArray { C.arrayRef = r, C.arrayLen = l }) =
  liftM CDArray $ replicateM (fromIntegral l) getter
  where
    getter = dynamicUnpackDetails m r

dynamicUnpackVector :: TyMap -> C.TVector -> S.LengthRepr -> Get CautDetails
dynamicUnpackVector m (C.TVector { C.vectorRef = r, C.vectorMaxLen = maxLen }) (S.LengthRepr lr) = do
  len <- unpackTag lr
  if len > maxLen
    then fail $ "vector length out of bounds: " ++ show len ++ " > " ++ show maxLen
    else liftM CDVector $ replicateM (fromIntegral len) getter
  where
    getter = dynamicUnpackDetails m r

dynamicUnpackRecord :: TyMap -> C.TRecord -> Get CautDetails
dynamicUnpackRecord m (C.TRecord { C.recordFields = C.Fields { C.unFields = fs } }) =
  liftM (CDRecord . M.fromList) $ mapM (unpackField m) fs

dynamicUnpackCombination :: TyMap -> C.TCombination -> S.FlagsRepr -> Get CautDetails
dynamicUnpackCombination m (C.TCombination { C.combinationFields = C.Fields { C.unFields = fs } }) (S.FlagsRepr fr) = do
  flags <- unpackTag fr
  liftM (CDCombination . M.fromList) $ mapM (unpackField m) (setFields flags)
  where
    setFields flags = filter (\f -> flags `testBit` (fromIntegral . C.fIndex $ f)) fs

dynamicUnpackUnion :: TyMap -> C.TUnion -> S.TagRepr -> Get CautDetails
dynamicUnpackUnion m (C.TUnion { C.unionFields = C.Fields { C.unFields = fs } }) (S.TagRepr { S.unTagRepr = tr } ) = do
  tag <- liftM fromIntegral $ unpackTag tr
  case tag `M.lookup` fm of
    Nothing -> fail $ "invalid union tag: " ++ show tag
    Just f -> do
      (n, d) <- unpackField m f
      return CDUnion { cdUnionFieldName = n, cdUnionFieldDetails = d }
  where
    fm = fieldsToIndexMap fs

unpackField :: TyMap -> C.Field -> Get (String, FieldValue)
unpackField _ (C.EmptyField { C.fName = n }) = return (n, EmptyField)
unpackField m (C.Field { C.fName = n, C.fRef = r }) =
  liftM (\d -> (n, DataField d)) (dynamicUnpackDetails m r)

unpackBuiltIn :: C.BuiltIn -> Get BIDetails
unpackBuiltIn b =
  case b of
    C.BIu8 -> liftM BDu8 getWord8
    C.BIu16 -> liftM BDu16 getWord16le
    C.BIu32 -> liftM BDu32 getWord32le
    C.BIu64 -> liftM BDu64 getWord64le
    C.BIs8 -> liftM (BDs8 . fromIntegral) getWord8
    C.BIs16 -> liftM (BDs16 . fromIntegral) getWord16le
    C.BIs32 -> liftM (BDs32 . fromIntegral) getWord32le
    C.BIs64 -> liftM (BDs64 . fromIntegral) getWord64le
    C.BIf32 -> liftM BDf32 getFloat32le
    C.BIf64 -> liftM BDf64 getFloat64le
    C.BIcu8 -> liftM BDcu8 getWord8
    C.BIcu16 -> liftM BDcu16 getWord16le
    C.BIcu32 -> liftM BDcu32 getWord32le
    C.BIbool -> do
      w8 <- getWord8
      case w8 of
        0 -> return $ BDbool False
        1 -> return $ BDbool True
        x -> fail $ "unexpected value for boolean: " ++ show x

unpackTag :: C.BuiltIn -> Get Integer
unpackTag C.BIu8  = liftM fromIntegral getWord8
unpackTag C.BIu16 = liftM fromIntegral getWord16le
unpackTag C.BIu32 = liftM fromIntegral getWord32le
unpackTag C.BIu64 = liftM fromIntegral getWord64le
unpackTag b = throw $ NotATagType (show b)
