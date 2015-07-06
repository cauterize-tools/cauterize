module Cauterize.Dynamic.Gen
  ( dynamicGen
  , dynamicGenType
  , dynamicGenType'
  ) where

import Cauterize.Dynamic.Common
import Cauterize.Dynamic.Types
import Control.Monad
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import qualified Cauterize.CommonTypes as C
import qualified Cauterize.Specification as S
import qualified Data.Map as M
import qualified Data.Text.Lazy as T

dynamicGen :: S.Specification -> IO CautType
dynamicGen s = do
  n <- generate $ elements $ M.keys m
  dynamicGenType s n
  where
    m = S.specTypeMap s

dynamicGenType :: S.Specification -> C.Identifier -> IO CautType
dynamicGenType s n = generate $ dynamicGenType' s n

dynamicGenType' :: S.Specification -> C.Identifier -> Gen CautType
dynamicGenType' s n = do
  d <- dynamicGenDetails m n
  return CautType { ctName = n, ctDetails = d }
  where
    m = S.specTypeMap s

dynamicGenDetails :: TyMap -> C.Identifier -> Gen CautDetails
dynamicGenDetails m n =
  case S.typeDesc (n `lu` m) of
   S.Synonym { S.synonymRef = sy } -> dynamicGenSynonym m sy
   S.Range { S.rangeOffset = o, S.rangeLength = l } -> dynamicGenRange o l
   S.Array { S.arrayRef = a, S.arrayLength = l } -> dynamicGenArray m a l
   S.Vector { S.vectorRef = v, S.vectorLength = l } -> dynamicGenVector m v l
   S.Enumeration { S.enumerationValues = vs } -> dynamicGenEnumeration vs
   S.Record { S.recordFields = r } -> dynamicGenRecord m r
   S.Combination { S.combinationFields = c } -> dynamicGenCombination m c
   S.Union { S.unionFields = u } -> dynamicGenUnion m u

dynamicGenPrim :: C.Prim -> Gen PrimDetails
dynamicGenPrim p =
  case p of
    C.PU8   -> liftM PDu8 arbitrary
    C.PU16  -> liftM PDu16 arbitrary
    C.PU32  -> liftM PDu32 arbitrary
    C.PU64  -> liftM PDu64 arbitrary
    C.PS8   -> liftM PDs8 arbitrary
    C.PS16  -> liftM PDs16 arbitrary
    C.PS32  -> liftM PDs32 arbitrary
    C.PS64  -> liftM PDs64 arbitrary
    C.PF32  -> liftM PDf32 arbitrary
    C.PF64  -> liftM PDf64 arbitrary
    C.PBool -> liftM PDbool arbitrary

dynamicGenSynonym :: TyMap -> C.Identifier -> Gen CautDetails
dynamicGenSynonym m s = liftM CDSynonym (dynamicGenDetails m s)

dynamicGenRange :: C.Offset -> C.Length -> Gen CautDetails
dynamicGenRange o l = do
  v <- choose (0, l)
  return $ CDRange (fromIntegral v + fromIntegral o)

dynamicGenArray :: TyMap -> C.Identifier -> C.Length -> Gen CautDetails
dynamicGenArray m r l =
  liftM CDArray $ replicateM (fromIntegral l) getter
  where
    getter = dynamicGenDetails m r

dynamicGenVector :: TyMap -> C.Identifier -> C.Length -> Gen CautDetails
dynamicGenVector m r maxLen = do
  len <- choose (0, maxLen)
  liftM CDVector $ replicateM (fromIntegral len) getter
  where
    getter = dynamicGenDetails m r

dynamicGenEnumeration :: [S.EnumVal] -> Gen CautDetails
dynamicGenEnumeration vs = liftM CDEnumeration (elements (map S.enumValName vs))

dynamicGenRecord :: TyMap -> [S.Field] -> Gen CautDetails
dynamicGenRecord m fs =
  liftM (CDRecord . M.fromList) $ mapM (genField m) fs

dynamicGenCombination :: TyMap -> [S.Field] -> Gen CautDetails
dynamicGenCombination m fs = do
  fields <- subset fs
  liftM (CDCombination . M.fromList) $ mapM (genField m) fields

dynamicGenUnion :: TyMap -> [S.Field] -> Gen CautDetails
dynamicGenUnion m fs = do
  f <- elements fs
  (n, d) <- genField m f
  return CDUnion { cdUnionFieldName = n, cdUnionFieldDetails = d }

genField :: TyMap -> S.Field -> Gen (C.Identifier, FieldValue)
genField _ (S.EmptyField { S.fieldName = n }) = return (n, EmptyField)
genField m (S.DataField  { S.fieldName = n, S.fieldRef = r }) =
  liftM (\d -> (n, DataField d)) (dynamicGenDetails m r)

subset :: [a] -> Gen [a]
subset [] = return []
subset (a:as) = do
  roll <- arbitrary
  if roll
    then liftM (a:) (subset as)
    else subset as

