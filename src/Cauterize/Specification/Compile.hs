{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Specification.Compile
  ( mkSpecification
  ) where

import Cauterize.CommonTypes
import Cauterize.Hash
import Cauterize.Specification.Types
import Data.Graph
import Data.Maybe
import qualified Cauterize.Schema.Types as Schema
import qualified Cauterize.Schema.Util as Schema
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T

mkSpecification :: Schema.IsSchema a => a -> Specification
mkSpecification schema = compile s
  where
    s = Schema.getSchema schema

compile :: Schema.Schema -> Specification
compile s@(Schema.Schema schemaName schemaVersion schemaTypes) = Specification
  { specName = schemaName
  , specVersion = schemaVersion
  , specFingerprint = mkSpecHash
  , specSize = mkSpecSize
  , specDepth = mkSpecDepth
  , specTypeLength = mkSpecTypeTag
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
    lud k = fromMaybe
             (error $ "compile: key '" ++ (T.unpack . unIdentifier) k ++ "' not in hash schemaDepthMap")
             (k `M.lookup` schemaDepthMap)

    convertType t@(Schema.Type sn d) =
      let tt = Schema.tagForType t
          (_,tha) = luh sn
          tsz = lus sn
          d' = case d of
                Schema.Synonym r       -> Synonym r
                Schema.Range o l       -> Range o l tt (primFittingAllInts [fromIntegral o, fromIntegral l + fromIntegral o])
                Schema.Array r l       -> Array r l
                Schema.Vector r l      -> Vector r l tt
                -- Enumeration tags start with 1 rather than 0 to
                -- avoid having the default initialization in C be a
                -- meaningful value.
                Schema.Enumeration vs  -> Enumeration (zipWith EnumVal vs [1..]) tt
                Schema.Record fs       -> Record (convertFields1 fs)
                Schema.Combination fs  -> Combination (convertFields0 fs) tt
                Schema.Union fs        -> Union (convertFields1 fs) tt
      in Type { typeName = sn
              , typeFingerprint = tha
              , typeSize = tsz
              , typeDesc = d'
              , typeDepth = lud sn
              }

    convertFields0 = zipWith convertField [0 ..]

    -- Field indicies start with 1 to avoid having the default
    -- initializer in C be a meaningful value. Really only useful for
    -- Unions but used for records as well.
    convertFields1 = zipWith convertField [1 ..]

    convertField ix (Schema.EmptyField sn) = EmptyField sn ix
    convertField ix (Schema.DataField sn r) = DataField sn ix r

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
