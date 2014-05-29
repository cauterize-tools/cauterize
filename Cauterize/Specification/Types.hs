{-# LANGUAGE FlexibleInstances, RecordWildCards #-}
module Cauterize.Specification.Types
  ( Spec(..)
  , SpType(..)
  , Sized(..)
  , FixedSize(..)
  , RangeSize(..)
  , fromSchema

  , prettyPrint
  ) where

import Cauterize.FormHash
import Cauterize.Common.Primitives
import Cauterize.Common.IndexedRef
import Data.List
import Data.Function
import Data.Maybe

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import qualified Cauterize.Schema.Types as SC

import Cauterize.Common.Types
import Cauterize.Common.References

import Text.PrettyPrint
import Text.PrettyPrint.Class

data FixedSize = FixedSize { unFixedSize :: Integer }
  deriving (Show, Ord, Eq)
data RangeSize = RangeSize { rangeSizeMin :: Integer, rangeSizeMax :: Integer }
  deriving (Show, Ord, Eq)

mkRangeSize :: Integer -> Integer -> RangeSize
mkRangeSize mi ma = if mi > ma
                      then error $ "Bad range: " ++ show mi ++ " -> " ++ show ma ++ "."
                      else RangeSize mi ma

class Sized a where
  minSize :: a -> Integer
  maxSize :: a -> Integer

  minimumOfSizes :: [a] -> Integer
  minimumOfSizes = minimum . map minSize

  maximumOfSizes :: [a] -> Integer
  maximumOfSizes = maximum . map maxSize

  rangeFitting :: [a] -> RangeSize
  rangeFitting ss = mkRangeSize (minimumOfSizes ss) (maximumOfSizes ss)

  sumOfMinimums :: [a] -> Integer
  sumOfMinimums = sum . map minSize

  sumOfMaximums :: [a] -> Integer
  sumOfMaximums = sum . map minSize

instance Sized FixedSize where
  minSize (FixedSize i) = i
  maxSize (FixedSize i) = i

instance Sized RangeSize where
  minSize (RangeSize i _) = i
  maxSize (RangeSize _ i) = i

instance Pretty FixedSize where
  pretty (FixedSize s) = parens $ text "fixed-size" <+> integer s

instance Pretty RangeSize where
  pretty (RangeSize mi ma) = parens $ text "range-size" <+> integer mi <+> integer ma

data Spec t = Spec Name Version FormHash RangeSize [SpType t]
  deriving (Show, Eq)

data SpType t = BuiltIn      { unBuiltIn   :: TBuiltIn
                             , spHash      :: FormHash
                             , spFixedSize :: FixedSize }

              | Scalar       { unScalar     :: TScalar
                             , spHash       :: FormHash
                             , spFixedSize  :: FixedSize }

              | Const        { unConst     :: TConst
                             , spHash      :: FormHash
                             , spFixedSize :: FixedSize }

              | FixedArray   { unFixed     :: TFixedArray t
                             , spHash      :: FormHash
                             , spRangeSize :: RangeSize }

              | BoundedArray { unBounded   :: TBoundedArray t
                             , spHash      :: FormHash
                             , spRangeSize :: RangeSize
                             , lenRepr   :: BuiltIn }

              | Struct       { unStruct    :: TStruct t
                             , spHash      :: FormHash
                             , spRangeSize :: RangeSize }

              | Set          { unSet       :: TSet t
                             , spHash      :: FormHash
                             , spRangeSize :: RangeSize
                             , flagsRepr   :: BuiltIn }

              | Enum         { unEnum      :: TEnum t
                             , spHash      :: FormHash
                             , spRangeSize :: RangeSize
                             , tagRepr     :: BuiltIn }

              | Partial      { unPartial   :: TPartial t
                             , spHash      :: FormHash
                             , spRangeSize :: RangeSize
                             , tagRepr     :: BuiltIn
                             , lenRepr     :: BuiltIn }

              | Pad          { unPad       :: TPad
                             , spHash      :: FormHash
                             , spFixedSize :: FixedSize }
  deriving (Show, Ord, Eq)

instance Sized (SpType b) where
  minSize (BuiltIn { spFixedSize = s}) = minSize s
  minSize (Scalar { spFixedSize = s}) = minSize s
  minSize (Const { spFixedSize = s}) = minSize s
  minSize (FixedArray { spRangeSize = s}) = minSize s
  minSize (BoundedArray { spRangeSize = s}) = minSize s
  minSize (Struct { spRangeSize = s}) = minSize s
  minSize (Set { spRangeSize = s}) = minSize s
  minSize (Enum { spRangeSize = s}) = minSize s
  minSize (Partial { spRangeSize = s}) = minSize s
  minSize (Pad { spFixedSize = s}) = minSize s

  maxSize (BuiltIn { spFixedSize = s}) = maxSize s
  maxSize (Scalar { spFixedSize = s}) = maxSize s
  maxSize (Const { spFixedSize = s}) = maxSize s
  maxSize (FixedArray { spRangeSize = s}) = maxSize s
  maxSize (BoundedArray { spRangeSize = s}) = maxSize s
  maxSize (Struct { spRangeSize = s}) = maxSize s
  maxSize (Set { spRangeSize = s}) = maxSize s
  maxSize (Enum { spRangeSize = s}) = maxSize s
  maxSize (Partial { spRangeSize = s}) = maxSize s
  maxSize (Pad { spFixedSize = s}) = maxSize s

spTypeName :: SpType a -> Name
spTypeName (BuiltIn { unBuiltIn = (TBuiltIn b)}) = show b
spTypeName (Scalar { unScalar = (TScalar n _)}) = n
spTypeName (Const { unConst = (TConst n _ _)}) = n
spTypeName (FixedArray { unFixed = (TFixedArray n _ _)}) = n
spTypeName (BoundedArray { unBounded = (TBoundedArray n _ _)}) = n
spTypeName (Struct { unStruct = (TStruct n _)}) = n
spTypeName (Set { unSet = (TSet n _)}) = n
spTypeName (Enum { unEnum = (TEnum n _)}) = n
spTypeName (Partial { unPartial = (TPartial n _)}) = n
spTypeName (Pad { unPad = (TPad n _)}) = n

pruneBuiltIns :: [SpType String] -> [SpType String]
pruneBuiltIns fs = refBis ++ topLevel
  where
    (bis, topLevel) = L.partition isBuiltIn fs

    biNames = map (\(BuiltIn (TBuiltIn b) _ _) -> show b) bis
    biMap = M.fromList $ zip biNames bis

    rsSet = S.fromList $ concatMap referencesOf topLevel
    biSet = S.fromList biNames

    refBiNames = S.toList $ rsSet `S.intersection` biSet
    refBis = map snd $ M.toList $ M.filterWithKey (\k _ -> k `elem` refBiNames) biMap
    
    isBuiltIn (BuiltIn {..}) = True
    isBuiltIn _ = False

-- TODO: Double-check the Schema hash can be recreated.
fromSchema :: SC.Schema Name -> Spec Name
fromSchema sc@(SC.Schema n v fs) = Spec n v overallHash (rangeFitting fs') fs'
  where
    fs' = pruneBuiltIns $ map fromF fs
    keepNames = S.fromList $ map spTypeName fs'

    tyMap = SC.schemaTypeMap sc
    sigMap = SC.schemaSigMap sc
    getSig t = fromJust $ t `M.lookup` sigMap
    hashScType = hashString . getSig . SC.typeName

    overallHash = let a = hashInit `hashUpdate` n `hashUpdate` v
                      sorted = sortBy (compare `on` fst) $ M.toList sigMap
                      filtered = filter (\(x,_) -> x `S.member` keepNames) sorted
                      hashStrs = map (show . hashString . snd) filtered
                  in hashFinalize $ foldl hashUpdate a hashStrs

    specMap = fmap fromF tyMap
    fromF p = mkSpecType specMap p hash
      where
        hash = hashScType p

mkSpecType :: M.Map Name (SpType Name) -> SC.ScType Name -> FormHash -> SpType Name
mkSpecType m p =
  case p of
    (SC.BuiltIn t@(TBuiltIn b)) ->
      let s = builtInSize b
      in \h -> BuiltIn t h (FixedSize s)
    (SC.Scalar  t@(TScalar _ b)) ->
      let s = builtInSize b
      in \h -> Scalar t h (FixedSize s)
    (SC.Const   t@(TConst _ b _)) ->
      let s = builtInSize b
      in \h -> Const t h (FixedSize s)
    (SC.FixedArray t@(TFixedArray _ r i)) ->
      let ref = lookupRef r
      in \h -> FixedArray t h (mkRangeSize (i * minSize ref) (i * maxSize ref))
    (SC.BoundedArray t@(TBoundedArray _ r i)) ->
      let ref = lookupRef r
          repr = minimalExpression i
          reprSz = builtInSize repr
      in \h -> BoundedArray t h (mkRangeSize reprSz (reprSz + (i * maxSize ref))) repr
    (SC.Struct t@(TStruct _ rs)) ->
      let refs = lookupRefs rs
          sumMin = sumOfMinimums refs
          sumMax = sumOfMaximums refs
      in \h -> Struct t h (mkRangeSize sumMin sumMax)
    (SC.Set t@(TSet _ rs)) ->
      let refs = lookupRefs rs
          sumMax = sumOfMaximums refs
          repr = minimalBitField (length rs)
          reprSz = builtInSize repr
      in \h -> Set t h (mkRangeSize reprSz (reprSz + sumMax)) repr
    (SC.Enum t@(TEnum _ rs)) ->
      let refs = lookupRefs rs
          minMin = minimumOfSizes refs
          maxMax = maximumOfSizes refs
          repr = minimalBitField (length rs)
          reprSz = builtInSize repr
      in \h -> Enum t h (mkRangeSize (reprSz + minMin) (reprSz + maxMax)) repr
    (SC.Partial t@(TPartial _ rs)) ->
      let refs = lookupRefs rs
          minMin = minimumOfSizes refs
          maxMax = maximumOfSizes refs
          ptagRepr = minimalExpression (length rs)
          ptagReprSz = builtInSize ptagRepr
          plenRepr = minimalExpression maxMax
          plenReprSz = builtInSize plenRepr
          overhead = ptagReprSz + plenReprSz
      in \h -> Partial t h (mkRangeSize (overhead + minMin) (overhead + maxMax)) ptagRepr plenRepr
    (SC.Pad t@(TPad _ l)) -> \h -> Pad t h (FixedSize l)
  where
    lookupRef r = fromJust $ r `M.lookup` m
    lookupIndexedRef (IndexedRef _ r _) = lookupRef r
    lookupRefs = map lookupIndexedRef

instance References (SpType String) where
  referencesOf (BuiltIn {..}) = []
  referencesOf (Scalar s _ _) = referencesOf s
  referencesOf (Const  c _ _) = referencesOf c
  referencesOf (FixedArray f _ _) = referencesOf f
  referencesOf (BoundedArray b _ _ r) = nub $ show r : referencesOf b
  referencesOf (Struct s _ _) = referencesOf s
  referencesOf (Set s _ _ r) = nub $ show r : referencesOf s
  referencesOf (Enum e _ _ r) = nub $ show r : referencesOf e
  referencesOf (Partial p _ _ r l) = nub $ show r : show l : referencesOf p
  referencesOf (Pad {..}) = []

prettyPrint :: Spec String -> String
prettyPrint = show . pretty

pShow :: (Show a) => a -> Doc
pShow = text . show 

pDQText :: String -> Doc
pDQText = doubleQuotes . text

instance Pretty (Spec String) where
  pretty (Spec n v h sz fs) = parens $ hang ps 1 pfs
    where
      ps = text "specification" <+> pDQText n <+> pDQText v <+> pretty sz <+> pretty h
      pfs = vcat $ map pretty fs

-- When printing spec types, the following is the general order of fields
--  (type name hash [references] [representations] [lengths])
instance Pretty (SpType String) where
  pretty (BuiltIn (TBuiltIn b) h sz) = parens $ pt <+> pa
    where
      pt = text "builtin" <+> pShow b <+> pretty h
      pa = pretty sz
  pretty (Scalar (TScalar n b) h sz) = parens $ pt $+$ nest 1 pa
    where
      pt = text "scalar" <+> text n <+> pretty h
      pa = pShow b <+> pretty sz
  pretty (Const (TConst n b i) h sz) = parens $ pt $+$ nest 1 pa
    where
      pt = text "const" <+> text n <+> pretty h
      pa = integer i <+> pShow b <+> pretty sz
  pretty (FixedArray (TFixedArray n m i) h sz) = parens $ pt $+$ nest 1 pa
    where
      pt = text "fixed" <+> text n <+> pretty h
      pa = text m <+> integer i <+> pretty sz
  pretty (BoundedArray (TBoundedArray n m i) h sz bi) = parens $ pt $+$ nest 1 pa
    where
      pt = text "bounded" <+> text n <+> pretty h
      pa = text m <+> integer i <+> pShow bi <+> pretty sz
  pretty (Struct (TStruct n rs) h sz) = prettyFieldedB0 "struct" n rs sz h
  pretty (Set (TSet n rs) h sz bi) = prettyFieldedB1 "set" n rs sz bi h
  pretty (Enum (TEnum n rs) h sz bi) = prettyFieldedB1 "enum" n rs sz bi h
  pretty (Partial (TPartial n rs) h sz bi ln) = prettyFieldedB2 "partial" n rs sz bi ln h
  -- when printing/parsing padding, the length of the padding is always the min/max
  pretty (Pad (TPad n _) h sz) = parens pt
    where
      pt = text "pad" <+> text n <+> pretty h <+> pretty sz

-- For fields, the representation is always:
--  (field [name] [target] [index])  
prettyIndexedRef :: IndexedRef String -> Doc
prettyIndexedRef (IndexedRef n m i) = parens $ text "field" <+> text n <+> text m <+> integer i

-- Printing fielded-types involves hanging the name, the sizes, and the hash on
-- one line and the fields on following lines.
prettyFieldedB0 :: String -> String -> [IndexedRef String] -> RangeSize -> FormHash -> Doc
prettyFieldedB0 t n fs sz hash = parens $ hang pt 1 pfs
  where
    pt = text t <+> text n <+> pretty sz <+> pretty hash
    pfs = vcat $ map prettyIndexedRef fs

prettyFieldedB1 :: String -> String -> [IndexedRef String] -> RangeSize -> BuiltIn -> FormHash -> Doc
prettyFieldedB1 t n fs sz repr hash = parens $ hang pt 1 pfs
  where
    pt = text t <+> text n <+> pretty sz <+> pShow repr <+> pretty hash
    pfs = vcat $ map prettyIndexedRef fs

prettyFieldedB2 :: String -> String -> [IndexedRef String] -> RangeSize -> BuiltIn -> BuiltIn -> FormHash -> Doc
prettyFieldedB2 t n fs sz repr1 repr2 hash = parens $ hang pt 1 pfs
  where
    pt = text t <+> text n <+> pretty sz <+> pShow repr1 <+> pShow repr2 <+> pretty hash
    pfs = vcat $ map prettyIndexedRef fs
