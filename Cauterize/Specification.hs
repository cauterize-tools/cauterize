{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Specification
  ( Spec(..)
  , SpecForm(..)
  , SpType(..)
  , fromSchema
  ) where

import Cauterize.FormHash
import Cauterize.Common.Primitives
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
              | SpStruct       (TStruct t)       FormHash
              | SpSet          (TSet t)          FormHash
              | SpEnum         (TEnum t)         FormHash
              | SpPartial      (TPartial t)      FormHash
              | SpPad          TPad              FormHash
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
    fromF p = whatIsThis specMap p hash
      where
        hash = hashScType p
      {-
      case p of
        c@(SC.ScBuiltIn t) -> whatSpBuiltIn t hash (spTypeSizes specMap c)
        c@(SC.ScScalar t) -> SpScalar t hash (spTypeSizes specMap c)
        c@(SC.ScConst t) -> SpConst t hash (spTypeSizes specMap c)
        c@(SC.ScFixedArray t) -> SpFixedArray t hash (spTypeSizes specMap c)
        c@(SC.ScBoundedArray t@(TBoundedArray _ _ i)) -> let lenRepr = minimalExpression i
                                                             (minDep, maxDep) = spTypeSizes c
                                                             reprLen = builtInSize lenRepr
                                                             baSizes = (reprLen + minDep, reprLen + maxDep)
                                                         in SpBoundedArray t hash baSizes
        SC.ScStruct t -> SpStruct t hash
        SC.ScSet t -> SpSet t hash
        SC.ScEnum t -> SpEnum t hash
        SC.ScPartial t -> SpPartial t hash
        SC.ScPad t -> SpPad t hash
        -}

spTypeSizeFields :: SpType t -> (MinSize, MaxSize)
spTypeSizeFields (SpBuiltIn      _ _ ss) = ss
spTypeSizeFields (SpScalar       _ _ ss) = ss
spTypeSizeFields (SpConst        _ _ ss) = ss
spTypeSizeFields (SpFixedArray   _ _ ss) = ss
spTypeSizeFields (SpBoundedArray _ _ ss _) = ss
spTypeSizeFields (SpStruct       _ _) = (0,0)
spTypeSizeFields (SpSet          _ _) = (0,0)
spTypeSizeFields (SpEnum         _ _) = (0,0)
spTypeSizeFields (SpPartial      _ _) = (0,0)
spTypeSizeFields (SpPad          _ _) = (0,0)

addSizes :: (MinSize, MaxSize) -> (MinSize, MaxSize) -> (MinSize, MaxSize)
addSizes (a, b) (c, d) = (a + c, b + d)



whatIsThis :: M.Map Name (SpType Name) -> SC.ScType Name -> (FormHash -> SpType Name)
whatIsThis m p =
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
      let (smin, smax) = spTypeSizeFields . fromJust $ r `M.lookup` m
      in \h -> SpFixedArray t h (i * smin, i * smax)

{-
instance SpecSized TScalar where
  sizes _ (TScalar _ b) = let s = builtInSize b in (s,s)

instance SpecSized TConst where
  sizes _ (TConst _ b _) = let s = builtInSize b in (s,s)

instance SpecSized (TFixedArray Name) where
  sizes m (TFixedArray _ t i) = let (mn, mx) = spTypeSizes . fromJust $ t `M.lookup` m in (i * mn, i * mx)

instance SpecSized (TBoundedArray Name) where
  sizes m (TFixedArray _ t i) = let (mn, mx) = spTypeSizes . fromJust $ t `M.lookup` m in (i * mn, i * mx)
  -}
