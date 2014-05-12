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

data SpType t = SpBuiltIn      TBuiltIn          FormHash
              | SpScalar       TScalar           FormHash
              | SpConst        TConst            FormHash
              | SpFixedArray   (TFixedArray t)   FormHash
              | SpBoundedArray (TBoundedArray t) FormHash
              | SpStruct       (TStruct t)       FormHash
              | SpSet          (TSet t)          FormHash
              | SpEnum         (TEnum t)         FormHash
              | SpPartial      (TPartial t)      FormHash
              | SpPad          TPad              FormHash
  deriving (Show, Ord, Eq)

fromSchema :: SC.Schema Name -> Spec Name
fromSchema sc@(SC.Schema n v fs) = Spec n v (map (FType . fromF . getT) fs)
  where
    sigMap = SC.schemaSigMap sc
    getSig t = fromJust $ t `M.lookup` sigMap
    hashScType = hashString . getSig . SC.typeName

    getT (SC.FType t) = t

    fromF :: SC.ScType Name -> SpType Name
    fromF s@(SC.ScBuiltIn t) = SpBuiltIn t $ hashScType s
    fromF s@(SC.ScScalar t) = SpScalar t $ hashScType s
    fromF s@(SC.ScConst t) = SpConst t $ hashScType s
    fromF s@(SC.ScFixedArray t) = SpFixedArray t $ hashScType s
    fromF s@(SC.ScBoundedArray t) = SpBoundedArray t $ hashScType s
    fromF s@(SC.ScStruct t) = SpStruct t $ hashScType s
    fromF s@(SC.ScSet t) = SpSet t $ hashScType s
    fromF s@(SC.ScEnum t) = SpEnum t $ hashScType s
    fromF s@(SC.ScPartial t) = SpPartial t $ hashScType s
    fromF s@(SC.ScPad t) = SpPad t $ hashScType s
