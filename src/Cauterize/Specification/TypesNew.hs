module Cauterize.Specification.TypesNew
  ( Specification(..)
  , Type(..)
  , TypeDesc(..)
  , Tag(..)
  , Field(..)
  ) where

import Cauterize.HashNew
import Cauterize.Specification.TypesInternalNew
import Data.Text (Text)
import Data.Word
import qualified Cauterize.Schema.TypesNew as Schema

-- TODO: these types probably belongs outside of the Schema module
import Cauterize.Schema.TypesNew (Identifier, Prim, Offset, Length)

data Specification = Specification
  { specName :: Text
  , specVersion :: Text
  , specHash :: Hash
  , specSize :: Size
  , specDepth :: Integer
  , specTypeTagWidth :: Integer
  , specLengthTagWidth :: Integer
  , specTypes :: [Type]
  } deriving (Show)

data Type = Type
  { typeName :: Identifier
  , typeHash :: Hash
  , typeSize :: Size
  } deriving (Show)

data TypeDesc
  = Synonym { synonymRef :: Identifier }
  | Range { rangeOffset :: Offset, rangeLength :: Length
          , rangeTag :: Tag }
  | Array { arrayRef :: Identifier, arrayLength :: Length }
  | Vector { vectorRef :: Identifier, vectorLength :: Length
           , vectorTag :: Tag }
  | Enumeration { enumerationValues :: [Identifier]
                , enumerationTag :: Tag }
  | Record { recordFields :: [Field] }
  | Combination { combinationFields :: [Field]
                , combinationTag :: Tag }
  | Union { unionFields :: [Field]
          , unionTag :: Tag}
  deriving (Show)

data Tag = T1 | T2 | T4 | T8
  deriving (Show, Eq)

data Field
  = DataField { fieldName :: Identifier, fieldIndex :: Integer, fieldRef :: Identifier }
  | EmptyField { fieldName :: Identifier, fieldIndex :: Integer }
  deriving (Show)

mkSpecification :: Schema.IsSchema a => a -> Specification
mkSpecification schema = undefined
  where
    s = Schema.getSchema schema

convTypes :: Schema.Schema -> [Type]
convTypes s@Schema.Schema { Schema.schemaTypes = ts } = undefined

