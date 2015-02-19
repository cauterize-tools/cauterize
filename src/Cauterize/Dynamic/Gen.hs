module Cauterize.Dynamic.Gen
  ( dynamicGen
  ) where

import Cauterize.Dynamic.Common
import Cauterize.Dynamic.Types
import Control.Monad
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import qualified Cauterize.Common.Types as C
import qualified Cauterize.Specification as S
import qualified Data.Map as M

dynamicGen :: S.Spec -> IO CautType
dynamicGen s = do
  n <- generate $ elements $ M.keys m
  dynamicGenType s n
  where
    m = S.specTypeMap s

dynamicGenType :: S.Spec -> String -> IO CautType
dynamicGenType s n = generate $ do
  d <- dynamicGenDetails m n
  return CautType { ctName = n, ctDetails = d }
  where
    m = S.specTypeMap s

dynamicGenDetails :: TyMap -> String -> Gen CautDetails
dynamicGenDetails m n =
  case n `lu` m of
   S.BuiltIn { S.unBuiltIn = bi } -> dynamicGenBuiltIn m bi
   S.Synonym { S.unSynonym = sy } -> dynamicGenSynonym m sy
   S.Array { S.unArray = a } -> dynamicGenArray m a
   S.Vector { S.unVector = v } -> dynamicGenVector m v
   S.Record { S.unRecord = r } -> dynamicGenRecord m r
   S.Combination { S.unCombination = c } -> dynamicGenCombination m c
   S.Union { S.unUnion = u } -> dynamicGenUnion m u

dynamicGenBuiltIn :: TyMap -> C.TBuiltIn -> Gen CautDetails
dynamicGenBuiltIn _ b = liftM CDBuiltIn (genBuiltIn $ C.unTBuiltIn b)

dynamicGenSynonym :: TyMap -> C.TSynonym -> Gen CautDetails
dynamicGenSynonym _ s = liftM CDSynonym (genBuiltIn $ C.synonymRepr s)

dynamicGenArray :: TyMap -> C.TArray -> Gen CautDetails
dynamicGenArray m (C.TArray { C.arrayRef = r, C.arrayLen = l }) =
  liftM CDArray $ replicateM (fromIntegral l) getter
  where
    getter = dynamicGenDetails m r

dynamicGenVector :: TyMap -> C.TVector -> Gen CautDetails
dynamicGenVector m (C.TVector { C.vectorRef = r, C.vectorMaxLen = maxLen }) = do
  len <- choose (0, maxLen)
  liftM CDVector $ replicateM (fromIntegral len) getter
  where
    getter = dynamicGenDetails m r

dynamicGenRecord :: TyMap -> C.TRecord -> Gen CautDetails
dynamicGenRecord m (C.TRecord { C.recordFields = C.Fields { C.unFields = fs } }) =
  liftM (CDRecord . M.fromList) $ mapM (genField m) fs

dynamicGenCombination :: TyMap -> C.TCombination -> Gen CautDetails
dynamicGenCombination m (C.TCombination { C.combinationFields = C.Fields { C.unFields = fs } }) = do
  fields <- subset fs
  liftM (CDCombination . M.fromList) $ mapM (genField m) fields

dynamicGenUnion :: TyMap -> C.TUnion -> Gen CautDetails
dynamicGenUnion m (C.TUnion { C.unionFields = C.Fields { C.unFields = fs } }) = do
  f <- elements fs
  (n, d) <- genField m f
  return CDUnion { cdUnionFieldName = n, cdUnionFieldDetails = d }

genBuiltIn :: C.BuiltIn -> Gen BIDetails
genBuiltIn b =
  case b of
    C.BIu8 -> liftM BDu8 arbitrary
    C.BIu16 -> liftM BDu16 arbitrary
    C.BIu32 -> liftM BDu32 arbitrary
    C.BIu64 -> liftM BDu64 arbitrary
    C.BIs8 -> liftM BDs8 arbitrary
    C.BIs16 -> liftM BDs16 arbitrary
    C.BIs32 -> liftM BDs32 arbitrary
    C.BIs64 -> liftM BDs64 arbitrary
    C.BIf32 -> liftM BDf32 arbitrary
    C.BIf64 -> liftM BDf64 arbitrary
    C.BIcu8 -> liftM BDcu8 arbitrary
    C.BIcu16 -> liftM BDcu16 arbitrary
    C.BIcu32 -> liftM BDcu32 arbitrary
    C.BIbool -> liftM BDbool arbitrary

genField :: TyMap -> C.Field -> Gen (String, FieldValue)
genField _ (C.EmptyField { C.fName = n }) = return (n, EmptyField)
genField m (C.Field { C.fName = n, C.fRef = r }) =
  liftM (\d -> (n, DataField d)) (dynamicGenDetails m r)

subset :: [a] -> Gen [a]
subset [] = return []
subset (a:as) = do
  roll <- arbitrary
  if roll
    then liftM (a:) (subset as)
    else subset as

