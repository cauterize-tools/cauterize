{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Specification.TypesNew
  ( Specification(..)
  , Type(..)
  , TypeDesc(..)
  , Field(..)
  , mkSpecification
  ) where

import Cauterize.CommonTypesNew
import Cauterize.HashNew
import Data.List (sort)
import Data.Maybe
import Data.Graph
import qualified Data.Text as T
import qualified Cauterize.Schema.TypesNew as Schema
import qualified Cauterize.Schema.UtilNew as Schema
import qualified Data.Map as M

data Specification = Specification
  { specName :: T.Text
  , specVersion :: T.Text
  , specHash :: Hash
  , specSize :: Size
  , specDepth :: Integer
  , specTypeTag :: Integer
  , specLengthTag :: Integer
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

data Field
  = DataField { fieldName :: Identifier, fieldIndex :: Integer, fieldRef :: Identifier }
  | EmptyField { fieldName :: Identifier, fieldIndex :: Integer }
  deriving (Show)

mkSpecification :: Schema.IsSchema a => a -> Specification
mkSpecification schema = compile s
  where
    s = Schema.getSchema schema

compile :: Schema.Schema -> Specification
compile s@(Schema.Schema n v ts) = Specification
  { specName = n
  , specVersion = v
  , specHash = mkSpecHash
  , specSize = mkSpecSize
  , specDepth = undefined -- mkSpecDepth s
  , specTypeTag = undefined -- mkSpecTypeTag s
  , specLengthTag = undefined -- mkSpecLengthTag s
  , specTypes = undefined -- topoSort $ map convertType ts
  }
  where
    schemaHashMap = Schema.typeHashMap s
    schemaSizeMap = Schema.typeSizeMap s

    sortedTypeNames = sort $ map Schema.typeName ts
    sortedTypeHashes = map (\tn -> fromJust $ tn `M.lookup` schemaHashMap) sortedTypeNames

    -- <name:version:[hash(t1)]:[hash(t2)]:...>
    mkSpecHash =
      let hstr = "<" `T.append` T.intercalate ":" (n : v : map (hashToHex . snd) sortedTypeHashes) `T.append` ">"
      in mkHash hstr

    mkSpecSize = let sizes = map snd $ M.toList schemaSizeMap
                     mins = map sizeMin sizes
                     maxes = map sizeMax sizes
                 in mkSize (minimum mins) (maximum maxes)

-- topoSort :: [Type] -> [Type]
-- topoSort sps = flattenSCCs . stronglyConnComp $ map m sps
--   where
--     m t = let n = typeName t
--           in (t, n, referencesOf t)
