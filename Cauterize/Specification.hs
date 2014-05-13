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

import Cauterize.Common.Types.BuiltIn
import Cauterize.Common.Types.Scalar
import Cauterize.Common.Types.Const
import Cauterize.Common.Types.FixedArray
import Cauterize.Common.Types.BoundedArray
import Cauterize.Common.Types.Struct
import Cauterize.Common.Types.Set
import Cauterize.Common.Types.Enum
import Cauterize.Common.Types.Partial
import Cauterize.Common.Types.Pad

data Spec t = Spec
  { specName :: Name
  , specVersion :: Version
  , specForms :: [SpecForm t]
  }
  deriving (Show)

data SpecForm t = FType (SpType t)
  deriving (Show)

data SpType t = SpBuiltIn      TBuiltIn          FormHash (MinSize, MaxSize)
              | SpScalar       TScalar           FormHash (MinSize, MaxSize)
              | SpConst        TConst            FormHash (MinSize, MaxSize)
              | SpFixedArray   (TFixedArray t)   FormHash (MinSize, MaxSize)
              | SpBoundedArray (TBoundedArray t) FormHash (MinSize, MaxSize) BuiltIn
              | SpStruct       (TStruct t)       FormHash (MinSize, MaxSize)
              | SpSet          (TSet t)          FormHash (MinSize, MaxSize) BuiltIn
              | SpEnum         (TEnum t)         FormHash (MinSize, MaxSize) BuiltIn
              | SpPartial      (TPartial t)      FormHash (MinSize, MaxSize) BuiltIn BuiltIn
              | SpPad          TPad              FormHash (MinSize, MaxSize)
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
spTypeSizeFields (SpBuiltIn      _ _ ss) = ss
spTypeSizeFields (SpScalar       _ _ ss) = ss
spTypeSizeFields (SpConst        _ _ ss) = ss
spTypeSizeFields (SpFixedArray   _ _ ss) = ss
spTypeSizeFields (SpBoundedArray _ _ ss _) = ss
spTypeSizeFields (SpStruct       _ _ ss) = ss
spTypeSizeFields (SpSet          _ _ ss _) = ss
spTypeSizeFields (SpEnum         _ _ ss _) = ss
spTypeSizeFields (SpPartial      _ _ ss _ _) = ss
spTypeSizeFields (SpPad          _ _ ss) = ss

mkSpecType :: M.Map Name (SpType Name) -> SC.ScType Name -> FormHash -> SpType Name
mkSpecType m p =
  case p of
    (SC.ScBuiltIn t@(TBuiltIn b)) ->
      let s = builtInSize b
      in \h -> SpBuiltIn t h (s,s)
    (SC.ScScalar  t@(TScalar _ b)) ->
      let s = builtInSize b
      in \h -> SpScalar t h (s,s)
    (SC.ScConst   t@(TConst _ b _)) ->
      let s = builtInSize b
      in \h -> SpConst t h (s,s)
    (SC.ScFixedArray t@(TFixedArray _ r i)) ->
      let (smin, smax) = lookupRef r
      in \h -> SpFixedArray t h (i * smin, i * smax)
    (SC.ScBoundedArray t@(TBoundedArray _ r i)) ->
      let (_, smax) = lookupRef r
          repr = minimalExpression i
          reprSz = builtInSize repr
      in \h -> SpBoundedArray t h (reprSz, reprSz + (i * smax)) repr
    (SC.ScStruct t@(TStruct _ rs)) ->
      let minMaxs = refsMinMaxes rs
          sumMin = sum $ map fst minMaxs
          sumMax = sum $ map snd minMaxs
      in \h -> SpStruct t h (sumMin, sumMax)
    (SC.ScSet t@(TSet _ rs)) ->
      let minMaxs = refsMinMaxes rs
          sumMax = sum $ map snd minMaxs
          repr = minimalBitField (length rs)
          reprSz = builtInSize repr
      in \h -> SpSet t h (reprSz, reprSz + sumMax) repr
    (SC.ScEnum t@(TEnum _ rs)) ->
      let minMaxs = refsMinMaxes rs
          minMin = minimum $ map fst minMaxs
          maxMax = maximum $ map snd minMaxs
          repr = minimalBitField (length rs)
          reprSz = builtInSize repr
      in \h -> SpEnum t h (reprSz + minMin, reprSz + maxMax) repr
    (SC.ScPartial t@(TPartial _ rs)) ->
      let minMaxs = refsMinMaxes rs
          minMin = minimum $ map fst minMaxs
          maxMax = maximum $ map snd minMaxs
          tagRepr = minimalExpression (length rs)
          tagReprSz = builtInSize tagRepr
          lenRepr = minimalExpression maxMax
          lenReprSz = builtInSize lenRepr
          overhead = tagReprSz + lenReprSz
      in \h -> SpPartial t h (overhead + minMin, overhead + maxMax) tagRepr lenRepr
    (SC.ScPad t@(TPad _ l)) -> \h -> SpPad t h (l, l)
  where
    lookupRef r = spTypeSizeFields . fromJust $ r `M.lookup` m
    lookupIndexedRef (IndexedRef _ r _) = lookupRef r
    refsMinMaxes = map lookupIndexedRef
