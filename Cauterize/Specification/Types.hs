{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Specification.Types
  ( Spec(..)
  , SpecForm(..)
  , SpType(..)
  , fromSchema
  ) where

import Cauterize.FormHash
import Cauterize.Common.Primitives
import Cauterize.Common.IndexedRef
import Data.Maybe

import qualified Data.Map as M
import qualified Cauterize.Schema.Types as SC

import Cauterize.Common.Types

data Spec t = Spec Name Version [SpecForm t]
  deriving (Show)

data SpecForm t = FType (SpType t)
  deriving (Show)

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

fromSchema :: SC.Schema Name -> Spec Name
fromSchema sc@(SC.Schema n v fs) = Spec n v (map (FType . fromF . getT) fs)
  where
    getT (SC.FType t) = t
    tyMap = SC.schemaTypeMap sc
    sigMap = SC.schemaSigMap sc
    getSig t = fromJust $ t `M.lookup` sigMap
    hashScType = hashString . getSig . SC.typeName

    specMap = fmap fromF tyMap

    fromF :: SC.ScType Name -> SpType Name
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

