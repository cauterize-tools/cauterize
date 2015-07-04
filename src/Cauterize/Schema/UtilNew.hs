{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Schema.UtilNew
  ( primToText
  , allPrimNames
  , schemaTypeNames
  , typeReferences
  , typeMap
  , typeHashMap
  ) where

import Cauterize.Schema.TypesNew
import Cauterize.HashNew
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T

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

typeMap :: IsSchema a => a -> M.Map Identifier Type
typeMap s = M.fromList pairs
  where
    ts = (schemaTypes . getSchema) s
    pairs = zip (map typeName ts) ts

typeHashMap :: IsSchema a => a -> M.Map Identifier (T.Text,Hash)
typeHashMap schema = th
  where
    s = getSchema schema
    tm = typeMap s
    th = primHashMap `M.union` fmap typeHash tm
    lu k = hashToHex . snd $
            fromMaybe
              (error $ "typeHashMap: key '" ++ (T.unpack . unIdentifier) k ++ "' not in map")
              (k `M.lookup` th)

    typeHash (Type n d) =
      let n' = unIdentifier n
          fmt _ [] = error "typeHashMap: fmt must have some parts"
          fmt p parts = T.concat ["<",p,":",T.intercalate ":" parts,">"]

          fmtField (DataField fn fr) = T.concat ["[datafield:", unIdentifier fn, ":", unIdentifier fr, ":", lu fr, "]"]
          fmtField (EmptyField fn) = T.concat ["[emptyfield:", unIdentifier fn, "]"]

          hstr =
            case d of
              Synonym r       -> fmt "synonym" [n',unIdentifier r,lu r]
              Range o l       -> fmt "range" [n',showNumSigned o,showNumSigned l]
              Array r l       -> fmt "array" [n',unIdentifier r,showNumSigned l]
              Vector r l      -> fmt "vector" [n',unIdentifier r,showNumSigned l]
              Enumeration vs  -> fmt "enumeration" (n':map unIdentifier vs)
              Record fs       -> fmt "record" (n':map fmtField fs)
              Combination fs  -> fmt "combination" (n':map fmtField fs)
              Union fs        -> fmt "union" (n':map fmtField fs)
      in (hstr, mkHash hstr)

primHashMap :: M.Map Identifier (T.Text,Hash)
primHashMap = M.fromList (zip allPrimNames vs)
  where
    ns = map unIdentifier allPrimNames
    hs = map mkHash ns
    vs = zip ns hs

showNumSigned :: (Ord a, Show a, Num a) => a -> T.Text
showNumSigned v = let v' = abs v
                      v'' = T.pack . show $ v'
                  in if v < 0
                       then '-' `T.cons` v''
                       else '+' `T.cons` v''
