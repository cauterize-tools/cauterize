{-# LANGUAGE FlexibleInstances, RecordWildCards #-}
module Cauterize.Specification.Types
  ( Spec(..)
  , SpType(..)
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

data Spec t = Spec Name Version FormHash (MinSize, MaxSize) [SpType t]
  deriving (Show, Eq)

data SpType t = BuiltIn      { unBuiltIn :: TBuiltIn
                             , spHash    :: FormHash
                             , spSizes   :: (MinSize, MaxSize) }
              | Scalar       { unScalar :: TScalar
                             , spHash   :: FormHash
                             , spSizes  :: (MinSize, MaxSize) }
              | Const        { unConst :: TConst
                             , spHash  :: FormHash
                             , spSizes :: (MinSize, MaxSize) }
              | FixedArray   { unFixed :: TFixedArray t
                             , spHash  :: FormHash
                             , spSizes :: (MinSize, MaxSize) }
              | BoundedArray { unBounded :: TBoundedArray t
                             , spHash    :: FormHash
                             , spSizes   :: (MinSize, MaxSize)
                             , lenRepr   :: BuiltIn }
              | Struct       { unStruct :: TStruct t
                             , spHash   :: FormHash
                             , spSizes  :: (MinSize, MaxSize) }
              | Set          { unSet     :: TSet t
                             , spHash    :: FormHash
                             , spSizes   :: (MinSize, MaxSize)
                             , flagsRepr :: BuiltIn }
              | Enum         { unEnum  :: TEnum t
                             , spHash  :: FormHash
                             , spSizes :: (MinSize, MaxSize)
                             , tagRepr :: BuiltIn }
              | Partial      { unPartial :: TPartial t
                             , spHash    :: FormHash
                             , spSizes   :: (MinSize, MaxSize)
                             , tagRepr   :: BuiltIn
                             , lenRepr   :: BuiltIn }
              | Pad          { unPad   :: TPad
                             , spHash  :: FormHash
                             , spSizes :: (MinSize, MaxSize) }
  deriving (Show, Ord, Eq)

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
fromSchema sc@(SC.Schema n v fs) = Spec n v overallHash (minimum minSizes, maximum maxSizes) fs'
  where
    (minSizes, maxSizes) = unzip $ map spSizes fs'
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
      in \h -> BuiltIn t h (s,s)
    (SC.Scalar  t@(TScalar _ b)) ->
      let s = builtInSize b
      in \h -> Scalar t h (s,s)
    (SC.Const   t@(TConst _ b _)) ->
      let s = builtInSize b
      in \h -> Const t h (s,s)
    (SC.FixedArray t@(TFixedArray _ r i)) ->
      let (smin, smax) = lookupRef r
      in \h -> FixedArray t h (i * smin, i * smax)
    (SC.BoundedArray t@(TBoundedArray _ r i)) ->
      let (_, smax) = lookupRef r
          repr = minimalExpression i
          reprSz = builtInSize repr
      in \h -> BoundedArray t h (reprSz, reprSz + (i * smax)) repr
    (SC.Struct t@(TStruct _ rs)) ->
      let minMaxs = refsMinMaxes rs
          sumMin = sum $ map fst minMaxs
          sumMax = sum $ map snd minMaxs
      in \h -> Struct t h (sumMin, sumMax)
    (SC.Set t@(TSet _ rs)) ->
      let minMaxs = refsMinMaxes rs
          sumMax = sum $ map snd minMaxs
          repr = minimalBitField (length rs)
          reprSz = builtInSize repr
      in \h -> Set t h (reprSz, reprSz + sumMax) repr
    (SC.Enum t@(TEnum _ rs)) ->
      let (minMin, maxMax) = minMinMaxMax rs
          repr = minimalBitField (length rs)
          reprSz = builtInSize repr
      in \h -> Enum t h (reprSz + minMin, reprSz + maxMax) repr
    (SC.Partial t@(TPartial _ rs)) ->
      let (minMin, maxMax) = minMinMaxMax rs
          ptagRepr = minimalExpression (length rs)
          ptagReprSz = builtInSize ptagRepr
          plenRepr = minimalExpression maxMax
          plenReprSz = builtInSize plenRepr
          overhead = ptagReprSz + plenReprSz
      in \h -> Partial t h (overhead + minMin, overhead + maxMax) ptagRepr plenRepr
    (SC.Pad t@(TPad _ l)) -> \h -> Pad t h (l, l)
  where
    lookupRef r = spSizes . fromJust $ r `M.lookup` m
    lookupIndexedRef (IndexedRef _ r _) = lookupRef r
    refsMinMaxes = map lookupIndexedRef
    minMinMaxMax rs = let minMaxs = refsMinMaxes rs
                          miMi = minimum $ map fst minMaxs
                          maMa = maximum $ map snd minMaxs
                      in (miMi, maMa)

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
  pretty (Spec n v h (minS, maxS) fs) = parens $ hang ps 1 pfs
    where
      ps = text "specification" <+> pDQText n <+> pDQText v <+> integer minS <+> integer maxS <+> pShow h
      pfs = vcat $ map pretty fs

-- When printing spec types, the following is the general order of fields
--  (type name hash [references] [representations] [lengths])
instance Pretty (SpType String) where
  pretty (BuiltIn (TBuiltIn b) h (sz,_)) = parens $ pt <+> pa
    where
      pt = text "builtin" <+> pShow b <+> pShow h
      pa = integer sz
  pretty (Scalar (TScalar n b) h (sz,_)) = parens $ pt $+$ nest 1 pa
    where
      pt = text "scalar" <+> text n <+> pShow h
      pa = pShow b <+> integer sz
  pretty (Const (TConst n b i) h (sz,_)) = parens $ pt $+$ nest 1 pa
    where
      pt = text "const" <+> text n <+> pShow h
      pa = integer i <+> pShow b <+> integer sz
  pretty (FixedArray (TFixedArray n m i) h (smin, smax)) = parens $ pt $+$ nest 1 pa
    where
      pt = text "fixed" <+> text n <+> pShow h
      pa = text m <+> integer i <+> integer smin <+> integer smax
  pretty (BoundedArray (TBoundedArray n m i) h (smin, smax) bi) = parens $ pt $+$ nest 1 pa
    where
      pt = text "bounded" <+> text n <+> pShow h
      pa = text m <+> integer i <+> pShow bi <+> integer smin <+> integer smax
  pretty (Struct (TStruct n rs) h (smin, smax)) = prettyFieldedB0 "struct" n rs smin smax h
  pretty (Set (TSet n rs) h (smin, smax) bi) = prettyFieldedB1 "set" n rs smin smax bi h
  pretty (Enum (TEnum n rs) h (smin, smax) bi) = prettyFieldedB1 "enum" n rs smin smax bi h
  pretty (Partial (TPartial n rs) h (smin, smax) bi ln) = prettyFieldedB2 "partial" n rs smin smax bi ln h
  -- when printing/parsing padding, the length of the padding is always the min/max
  pretty (Pad (TPad n l) h (_, _)) = parens pt
    where
      pt = text "pad" <+> text n <+> pShow h <+> integer l

-- For fields, the representation is always:
--  (field [name] [target] [index])  
prettyIndexedRef :: IndexedRef String -> Doc
prettyIndexedRef (IndexedRef n m i) = parens $ text "field" <+> text n <+> text m <+> integer i

-- Printing fielded-types involves hanging the name, the sizes, and the hash on
-- one line and the fields on following lines.
prettyFieldedB0 :: String -> String -> [IndexedRef String] -> MinSize -> MaxSize -> FormHash -> Doc
prettyFieldedB0 t n fs smin smax hash = parens $ hang pt 1 pfs
  where
    pt = text t <+> text n <+> integer smin <+> integer smax <+> pShow hash
    pfs = vcat $ map prettyIndexedRef fs

prettyFieldedB1 :: String -> String -> [IndexedRef String] -> MinSize -> MaxSize -> BuiltIn -> FormHash -> Doc
prettyFieldedB1 t n fs smin smax repr hash = parens $ hang pt 1 pfs
  where
    pt = text t <+> text n <+> integer smin <+> integer smax <+> pShow repr <+> pShow hash
    pfs = vcat $ map prettyIndexedRef fs

prettyFieldedB2 :: String -> String -> [IndexedRef String] -> MinSize -> MaxSize -> BuiltIn -> BuiltIn -> FormHash -> Doc
prettyFieldedB2 t n fs smin smax repr1 repr2 hash = parens $ hang pt 1 pfs
  where
    pt = text t <+> text n <+> integer smin <+> integer smax <+> pShow repr1 <+> pShow repr2 <+> pShow hash
    pfs = vcat $ map prettyIndexedRef fs
