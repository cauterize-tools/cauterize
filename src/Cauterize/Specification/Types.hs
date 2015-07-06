{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Specification.Types
  ( Specification(..)
  , Type(..)
  , TypeDesc(..)
  , Field(..)
  , EnumVal(..)
  ) where

import Cauterize.CommonTypes
import Cauterize.Hash
import qualified Cauterize.Schema.Types as Schema
import qualified Data.Text as T

data Specification = Specification
  { specName :: T.Text
  , specVersion :: T.Text
  , specFingerprint :: Hash
  , specSize :: Size
  , specDepth :: Integer
  , specTypeLength :: Integer
  , specLengthTag :: Tag
  , specTypes :: [Type]
  } deriving (Show)

data Type = Type
  { typeName :: Identifier
  , typeFingerprint :: Hash
  , typeSize :: Size
  , typeDesc :: TypeDesc
  } deriving (Show)

data TypeDesc
  = Synonym { synonymRef :: Identifier }
  | Range { rangeOffset :: Offset, rangeLength :: Length
          , rangeTag :: Tag }
  | Array { arrayRef :: Identifier, arrayLength :: Length }
  | Vector { vectorRef :: Identifier, vectorLength :: Length
           , vectorTag :: Tag }
  | Enumeration { enumerationValues :: [EnumVal]
                , enumerationTag :: Tag }
  | Record { recordFields :: [Field] }
  | Combination { combinationFields :: [Field]
                , combinationTag :: Tag }
  | Union { unionFields :: [Field]
          , unionTag :: Tag}
  deriving (Show)

data Field
  = DataField { fieldName :: Identifier, fieldIndex :: Integer, fieldRef :: Identifier }
  | EmptyField { fieldName :: Identifier, fieldIndex :: Integer }
  deriving (Show)

data EnumVal = EnumVal { enumValName :: Identifier, enumValIndex :: Integer }
  deriving (Show)


instance Schema.IsSchema Specification where
  getSchema spec =
    Schema.Schema
      { Schema.schemaName = specName spec
      , Schema.schemaVersion = specVersion spec
      , Schema.schemaTypes = map extractType (specTypes spec)
      }
    where
      extractType (Type n _ _ d) =
        let
          d' = case d of
                Synonym r        -> Schema.Synonym r
                Range o l _      -> Schema.Range o l
                Array r l        -> Schema.Array r l
                Vector r l _     -> Schema.Vector r l
                Enumeration vs _ -> Schema.Enumeration (map enumValName vs)
                Record fs        -> Schema.Record (map extractField fs)
                Combination fs _ -> Schema.Combination (map extractField fs)
                Union fs _       -> Schema.Union (map extractField fs)
        in Schema.Type n d'

      extractField (EmptyField n _)  = Schema.EmptyField n
      extractField (DataField n _ r) = Schema.DataField n r
