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
import Data.Graph
import Data.Maybe
import qualified Cauterize.Schema.TypesNew as Schema
import qualified Cauterize.Schema.UtilNew as Schema
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T

data Specification = Specification
  { specName :: T.Text
  , specVersion :: T.Text
  , specHash :: Hash
  , specSize :: Size
  , specDepth :: Integer
  , specTypeTagLength :: Integer
  , specLengthTag :: Tag
  , specTypes :: [Type]
  } deriving (Show)

data Type = Type
  { typeName :: Identifier
  , typeHash :: Hash
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
compile s@(Schema.Schema schemaName schemaVersion schemaTypes) = Specification
  { specName = schemaName
  , specVersion = schemaVersion
  , specHash = mkSpecHash
  , specSize = mkSpecSize
  , specDepth = mkSpecDepth
  , specTypeTagLength = mkSpecTypeTag
  , specLengthTag = mkSpecLengthTag
  , specTypes = topoSort schemaTypeMap $ map convertType schemaTypes
  }
  where
    schemaTypeMap = Schema.typeMap s
    schemaHashMap = Schema.typeHashMap s
    schemaSizeMap = Schema.typeSizeMap s
    schemaDepthMap = Schema.typeDepthMap s

    sortedTypeNames = L.sort $ map Schema.typeName schemaTypes
    sortedTypeHashes = map (\tn -> fromJust $ tn `M.lookup` schemaHashMap) sortedTypeNames

    -- <name:version:[hash(t1)]:[hash(t2)]:...>
    mkSpecHash =
      let hstr = "<" `T.append` T.intercalate ":" (schemaName : schemaVersion : map (hashToHex . snd) sortedTypeHashes) `T.append` ">"
      in mkHash hstr

    mkSpecSize = let sizes = map snd $ M.toList schemaSizeMap
                     mins = map sizeMin sizes
                     maxes = map sizeMax sizes
                 in mkSize (minimum mins) (maximum maxes)

    mkSpecDepth = maximum (map snd $ M.toList schemaDepthMap)

    mkSpecLengthTag = tagRequired (sizeMax mkSpecSize)

    mkSpecTypeTag = let prefixes = uniquePrefixes $ map (hashToBytes . snd . snd) (M.toList schemaHashMap)
                    in case prefixes of
                         Just (p:_) -> fromIntegral $ length p
                         _ -> error "Need at least one prefix to determine a prefix length"

    luh k = fromMaybe
             (error $ "compile: key '" ++ (T.unpack . unIdentifier) k ++ "' not in hash schemaHashMap")
             (k `M.lookup` schemaHashMap)
    lus k = fromMaybe
             (error $ "compile: key '" ++ (T.unpack . unIdentifier) k ++ "' not in hash schemaSizeMap")
             (k `M.lookup` schemaSizeMap)

    convertType t@(Schema.Type sn d) =
      let tt = Schema.tagForType t
          (_,tha) = luh sn
          tsz = lus sn
          d' = case d of
                Schema.Synonym r       -> Synonym r
                Schema.Range o l       -> Range o l tt
                Schema.Array r l       -> Array r l
                Schema.Vector r l      -> Vector r l tt
                Schema.Enumeration vs  -> Enumeration vs tt
                Schema.Record fs       -> Record (convertFields fs)
                Schema.Combination fs  -> Combination (convertFields fs) tt
                Schema.Union fs        -> Union (convertFields fs) tt
      in Type sn tha tsz d'

    convertFields fs = map convertField (zip [0..] fs)

    convertField (ix, Schema.EmptyField sn) = EmptyField sn ix
    convertField (ix, Schema.DataField sn r) = DataField sn ix r

topoSort :: M.Map Identifier Schema.Type -> [Type] -> [Type]
topoSort m types = flattenSCCs . stronglyConnComp $ map mkNode types
  where
    lu k = fromMaybe
            (error $ "topoSort: key '" ++ (T.unpack . unIdentifier) k ++ "' not in schema type map")
            (k `M.lookup` m)
    mkNode t = let n = typeName t
               in (t, n, Schema.typeReferences $ lu n)

uniquePrefixes :: Eq a => [[a]] -> Maybe [[a]]
uniquePrefixes ls = let count = length ls
                    in case dropWhile (\l -> length l < count) $ map L.nub $ L.transpose $ map L.inits ls of
                          [] -> Nothing
                          l -> (Just . head) l
