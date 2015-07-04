{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Schema.UtilNew
  ( primToText
  , allPrimNames
  , schemaTypeNames
  , typeReferences
  , typeMap
  ) where

import Cauterize.Schema.TypesNew
import Data.Map (Map, fromList)

primToText :: Prim -> Identifier
primToText PU8    = "u8"
primToText PU16   = "u16"
primToText PU32   = "u32"
primToText PU64   = "u64"
primToText PS8    = "s8"
primToText PS16   = "s16"
primToText PS32   = "s32"
primToText PS64   = "s64"
primToText PF32   = "f32"
primToText PF64   = "f64"
primToText PBool  = "bool"

allPrimNames :: [Identifier]
allPrimNames = map primToText [minBound..maxBound]

schemaTypeNames :: IsSchema a => a -> [Identifier]
schemaTypeNames s =
  let types = (schemaTypes . getSchema) s
  in map typeName types

typeReferences :: Type -> [Identifier]
typeReferences (Type _ d) = go d
  where
    go (Synonym r) = [r]
    go (Range _ _) = []
    go (Array r _) = [r]
    go (Vector r _) = [r]
    go (Enumeration _) = []
    go (Record fs) = concatMap fieldReference fs
    go (Combination fs) = concatMap fieldReference fs
    go (Union fs) = concatMap fieldReference fs

    fieldReference (DataField _ r) = [r]
    fieldReference (EmptyField _) = []

typeMap :: IsSchema a => a -> Map Identifier Type
typeMap s = fromList pairs
  where
    ts = (schemaTypes . getSchema) s
    pairs = zip (map typeName ts) ts
