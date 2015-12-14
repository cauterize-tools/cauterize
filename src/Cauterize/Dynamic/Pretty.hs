{-# LANGUAGE OverloadedStrings, CPP #-}
module Cauterize.Dynamic.Pretty
  ( dynamicPretty
  ) where

#if __GLASGOW_HASKELL__ >= 710
import Prelude hiding ((<$>))
#endif

import Cauterize.CommonTypes
import Cauterize.Dynamic.Types as DT
import Cauterize.Specification
import Data.Maybe
import Text.PrettyPrint.Leijen.Text
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

dynamicPretty :: Specification -> CautType -> T.Text
dynamicPretty s t = T.concat $ TL.toChunks (displayT $ renderPretty 0.9 80 $ dp s t)

dp :: Specification -> CautType -> Doc
dp s (CautType n d) = prettyDetails sm n d
  where
    sm = specTypeMap s

lu :: Identifier -> M.Map Identifier a -> a
lu k m = fromMaybe (error $ "key doesn't exist: " ++ T.unpack (unIdentifier k))
                   (k `M.lookup` m)

ident :: Identifier -> Doc
ident = text . TL.fromChunks . (:[]) .  unIdentifier

prettyDetails :: M.Map Identifier Type -> Identifier -> CautDetails -> Doc
prettyDetails m n (CDSynonym s) = parens $ "synonym" <+> prettyDetails m r s
  where
    Type { typeDesc = (Synonym { synonymRef = r }) } = n `lu` m
prettyDetails _ n (CDRange v) =
  parens ("range" <+> ident n <$> ii v)
prettyDetails s n (CDArray elems) =
  parens ("array" <+> ident n <$> indent 2 (parens $ "elems" <$> indent 2 vs))
  where
    vs = fillSep $ map (prettyDetails s elemsName) elems
    Type { typeDesc = Array { arrayRef = elemsName } } = n `lu` s
prettyDetails s n (CDVector elems) =
  parens ("vector" <+> ident n <$> indent 2 (parens $ "elems" <$> indent 2 vs))
  where
    vs = fillSep $ map (prettyDetails s elemsName) elems
    Type { typeDesc = Vector { vectorRef = elemsName } } = n `lu` s
prettyDetails _ n (CDEnumeration v) =
  parens ("enumeration" <+> ident n <$> ident v)
prettyDetails s n (CDRecord fields) =
  parens ("record" <+> ident n <$> indent 2 fs)
    where
      fs = sep $ map prettyRecField (M.toList fields)
      prettyRecField m = prettyField s n m (recordFields . typeDesc)
prettyDetails s n (CDCombination fields) =
  parens ("combination" <+> ident n <$> indent 2 fs)
    where
      fs = sep $ map prettyCombField (M.toList fields)
      prettyCombField m = prettyField s n m (combinationFields . typeDesc)
prettyDetails s n (CDUnion unionFieldName unionFieldValue) =
  parens ("union" <+> ident n <$> indent 2 (prettyUnionField unionFieldName unionFieldValue))
  where
    prettyUnionField fn fv = prettyField s n (fn, fv) (unionFields . typeDesc)
prettyDetails _ _ (CDPrim p) = prettyPrim p

prettyPrim :: PrimDetails -> Doc
prettyPrim d = parens (n <+> val)
  where
    (n, val) = case d of
                PDu8 v   -> ("u8", ii v)
                PDu16 v  -> ("u16", ii v)
                PDu32 v  -> ("u32", ii v)
                PDu64 v  -> ("u64", ii v)
                PDs8 v   -> ("s8", ii v)
                PDs16 v  -> ("s16", ii v)
                PDs32 v  -> ("s32", ii v)
                PDs64 v  -> ("s64", ii v)
                PDf32 v  -> ("f32", float v)
                PDf64 v  -> ("f64", double v)
                PDbool v -> ("bool", bool v)

ii :: Integral a => a -> Doc
ii = integer . fromIntegral

prettyField :: M.Map Identifier Type -- the spec type map
            -> Identifier -- the field's parent type name
            -> (Identifier, FieldValue) -- the name/field-value pair to pretty print
            -> (Type -> [Field]) -- unwraps a record/combination/union into fields
            -> Doc
prettyField _ _ (fn, DT.EmptyField) _ = parens $ "empty" <+> ident fn
prettyField s n (fn, DT.DataField fd) unwrap =
  parens $ "field" <+> ident fn <$> indent 2 (prettyDetails s (typeName ft) fd)
  where
    fm = fieldsMap . unwrap $ n `lu` s
    ftn = fn `lu` fm
    ft = fieldRef ftn `lu` s

fieldsMap :: [Field] -> M.Map Identifier Field
fieldsMap fs = M.fromList $ zip (map fieldName fs) fs
