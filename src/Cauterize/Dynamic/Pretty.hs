{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Dynamic.Pretty
  ( dynamicPretty
  ) where

import Cauterize.Common.Types
import Cauterize.Dynamic.Types as DT
import Cauterize.Specification
import Data.Maybe
import Text.PrettyPrint.Leijen.Text
import qualified Data.Map as M
import qualified Data.Text.Lazy as T

dynamicPretty :: Spec -> CautType -> T.Text
dynamicPretty s t = displayT $ renderPretty 0.1 120 $ dp s t

dp :: Spec -> CautType -> Doc
dp s (CautType n d) = prettyDetails sm n d
  where
    sm = specTypeMap s

lu :: T.Text -> M.Map T.Text a -> a
lu k m = fromMaybe (error $ "key doesn't exist: " ++ T.unpack k)
                   (k `M.lookup` m)

prettyDetails :: M.Map T.Text SpType -> T.Text -> CautDetails -> Doc
prettyDetails _ n (CDBuiltIn bid) = parens $ "builtin" <+> text n <+> prettyBuiltIn bid
prettyDetails _ n (CDSynonym bid) = parens $ "synonym" <+> text n <+> prettyBuiltIn bid
prettyDetails s n (CDArray elems) =
  parens ("array" <+> text n <$> indent 2 (parens $ "elems" <$> indent 2 vs))
  where
    vs = sep $ map (prettyDetails s elemsName) elems
    Array { unArray = TArray { arrayRef = elemsName } } = n `lu` s
prettyDetails s n (CDVector elems) =
  parens ("vector" <+> text n <$> indent 2 (parens $ "elems" <$> indent 2 vs))
  where
    vs = sep $ map (prettyDetails s elemsName) elems
    Vector { unVector = TVector { vectorRef = elemsName } } = n `lu` s
prettyDetails s n (CDRecord fields) =
  parens ("record" <+> text n <$> indent 2 (parens $ "fields" <$> indent 2 fs))
    where
      fs = sep $ map prettyRecField (M.toList fields)
      prettyRecField m = prettyField s n m (recordFields . unRecord)
prettyDetails s n (CDCombination fields) =
  parens ("combination" <+> text n <$> indent 2 (parens $ "fields" <$> indent 2 fs))
    where
      fs = sep $ map prettyCombField (M.toList fields)
      prettyCombField m = prettyField s n m (combinationFields . unCombination)
prettyDetails s n (CDUnion fieldName fieldValue) =
  parens ("union" <+> text n <$> indent 2 (prettyUnionField fieldName fieldValue))
  where
    prettyUnionField fn fv = prettyField s n (fn, fv) (unionFields . unUnion)


prettyBuiltIn :: BIDetails -> Doc
prettyBuiltIn (BDu8 v) = integer $ fromIntegral v
prettyBuiltIn (BDu16 v) = integer $ fromIntegral v
prettyBuiltIn (BDu32 v) = integer $ fromIntegral v
prettyBuiltIn (BDu64 v) = integer $ fromIntegral v
prettyBuiltIn (BDs8 v) = integer $ fromIntegral v
prettyBuiltIn (BDs16 v) = integer $ fromIntegral v
prettyBuiltIn (BDs32 v) = integer $ fromIntegral v
prettyBuiltIn (BDs64 v) = integer $ fromIntegral v
prettyBuiltIn (BDf32 v) = float v
prettyBuiltIn (BDf64 v) = double v
prettyBuiltIn (BDbool v) = bool v

prettyField :: M.Map T.Text SpType -- the spec type map
            -> T.Text -- the field's parent type name
            -> (T.Text, FieldValue) -- the name/field-value pair to pretty print
            -> (SpType -> Fields) -- unwraps a record/combination/union into fields
            -> Doc
prettyField _ _ (_, DT.EmptyField) _ = empty
prettyField s n (fn, DT.DataField fd) unwrap =
  parens $ "field" <+> text fn <$> indent 2 (prettyDetails s (typeName ft) fd)
  where
    fm = fieldsMap . unwrap $ n `lu` s
    ftn = fn `lu` fm
    ft = fRef ftn `lu` s

fieldsMap :: Fields -> M.Map T.Text Field
fieldsMap (Fields fs) = M.fromList $ zip (map fName fs) fs
