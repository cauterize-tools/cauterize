module Cauterize.Specification
  ( module Cauterize.Specification.Types
  , fromSchema
  ) where

import Cauterize.Specification.Types
import Cauterize.FormHash
import Cauterize.Common.BuiltIn
import Cauterize.Common.Named

import Data.Bits
import Data.List
import Data.Maybe

import qualified Data.Map as M
import qualified Cauterize.Schema as SC

fromSchema :: SC.Schema -> Maybe Specification
fromSchema schema@(SC.Schema n v fs) =
  let (Right tidMap) = SC.schemaTypeIdMap schema
      spec = Specification n v (show $ SC.schemaStructuralHash schema) (fromSchemaForms tidMap fs)
  in Just spec

fromSchemaForms :: SC.TypeIdMap -> [SC.SchemaForm] -> [SpecForm]
fromSchemaForms m = map fromSchemaForm
  where
    fromSchemaForm (SC.FType f) = SpecForm $ fromSchemaType m f

fromSchemaType :: SC.TypeIdMap -> SC.Type -> Type
fromSchemaType tyMap t =
    case t of
      (SC.TBuiltIn b) -> TBuiltIn b tid
      (SC.TScalar n b) -> TScalar n b tid
      (SC.TConst n b i) -> TConst n b i tid
      (SC.TFixedArray n m i) -> TFixedArray n m i tid
      (SC.TBoundedArray n m i) -> TBoundedArray n m i (minimalExpression i) tid
      (SC.TStruct n fs) -> TStruct n (fromSchemaStructFields fs) tid
      (SC.TSet n fs) -> TSet n (minimalBitField $ length fs) (fromSchemaSetFields fs) tid
      (SC.TEnum n vs) -> TEnum n (minimalExpression $ length vs) (fromSchemaEnumVariants vs) tid
      (SC.TPartial n l vs) -> TPartial n l (minimalExpression l) (fromSchemaPartialVariants tyMap vs) tid
      (SC.TPad n l) -> TPad n l tid
      
  where
    tid :: FormHash
    tid = fromJust $ cautName t `M.lookup` tyMap

fromSchemaStructFields :: [SC.StructField] -> [StructField]
fromSchemaStructFields = map go
  where
    go (SC.StructField n t) = StructField n t

fromSchemaSetFields :: [SC.SetField] -> [SetField]
fromSchemaSetFields fs = snd $ mapAccumL go 0 fs
  where
    go :: Int -> SC.SetField -> (Int, SetField)
    go a (SC.SetField n m) = (a + 1, SetField n m (1 `shiftL` a))

fromSchemaEnumVariants :: [SC.EnumVariant] -> [EnumVariant]
fromSchemaEnumVariants vs = snd $ mapAccumL go 0 vs
  where
    go :: Int -> SC.EnumVariant -> (Int, EnumVariant)
    go a (SC.EnumVariant n m) = (a + 1, EnumVariant n m (fromIntegral a))

fromSchemaPartialVariants :: SC.TypeIdMap -> [SC.PartialVariant] -> [PartialVariant]
fromSchemaPartialVariants tyMap = map go
  where
    go :: SC.PartialVariant -> PartialVariant
    go (SC.PartialVariant n t) =
      let tyId = fromJust $ t `M.lookup` tyMap
      in PartialVariant n t (finalize $ hashInit `hashFn` n `formHashWith` tyId)
