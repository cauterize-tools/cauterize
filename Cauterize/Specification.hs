{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Specification
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

data SpType t = BuiltIn      TBuiltIn          FormHash (MinSize, MaxSize)
              | Scalar       TScalar           FormHash (MinSize, MaxSize)
              | Const        TConst            FormHash (MinSize, MaxSize)
              | FixedArray   (TFixedArray t)   FormHash (MinSize, MaxSize)
              | BoundedArray (TBoundedArray t) FormHash (MinSize, MaxSize) BuiltIn
              | Struct       (TStruct t)       FormHash (MinSize, MaxSize)
              | Set          (TSet t)          FormHash (MinSize, MaxSize) BuiltIn
              | Enum         (TEnum t)         FormHash (MinSize, MaxSize) BuiltIn
              | Partial      (TPartial t)      FormHash (MinSize, MaxSize) BuiltIn BuiltIn
              | Pad          TPad              FormHash (MinSize, MaxSize)
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

spTypeSizeFields :: SpType t -> (MinSize, MaxSize)
spTypeSizeFields (BuiltIn      _ _ ss) = ss
spTypeSizeFields (Scalar       _ _ ss) = ss
spTypeSizeFields (Const        _ _ ss) = ss
spTypeSizeFields (FixedArray   _ _ ss) = ss
spTypeSizeFields (BoundedArray _ _ ss _) = ss
spTypeSizeFields (Struct       _ _ ss) = ss
spTypeSizeFields (Set          _ _ ss _) = ss
spTypeSizeFields (Enum         _ _ ss _) = ss
spTypeSizeFields (Partial      _ _ ss _ _) = ss
spTypeSizeFields (Pad          _ _ ss) = ss

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
          tagRepr = minimalExpression (length rs)
          tagReprSz = builtInSize tagRepr
          lenRepr = minimalExpression maxMax
          lenReprSz = builtInSize lenRepr
          overhead = tagReprSz + lenReprSz
      in \h -> Partial t h (overhead + minMin, overhead + maxMax) tagRepr lenRepr
    (SC.Pad t@(TPad _ l)) -> \h -> Pad t h (l, l)
  where
    lookupRef r = spTypeSizeFields . fromJust $ r `M.lookup` m
    lookupIndexedRef (IndexedRef _ r _) = lookupRef r
    refsMinMaxes = map lookupIndexedRef
    minMinMaxMax rs = let minMaxs = refsMinMaxes rs
                          miMi = minimum $ map fst minMaxs
                          maMa = maximum $ map snd minMaxs
                      in (miMi, maMa)
