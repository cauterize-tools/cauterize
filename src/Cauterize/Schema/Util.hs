{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Schema.Util
  ( schemaTypeNames
  , typeReferences
  , typeMap
  , typeHashMap
  , typeSizeMap
  , typeDepthMap
  , tagForType
  ) where

import Cauterize.Schema.Types
import Cauterize.Hash
import Cauterize.CommonTypes
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T

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
              Array r l       -> fmt "array" [n',unIdentifier r,showNumSigned l, lu r]
              Vector r l      -> fmt "vector" [n',unIdentifier r,showNumSigned l, lu r]
              Enumeration vs  -> fmt "enumeration" (n':map unIdentifier vs)
              Record fs       -> fmt "record" (n':map fmtField fs)
              Combination fs  -> fmt "combination" (n':map fmtField fs)
              Union fs        -> fmt "union" (n':map fmtField fs)
      in (hstr, mkHash hstr)

typeSizeMap :: IsSchema a => a -> M.Map Identifier Size
typeSizeMap schema = sm
  where
    s = getSchema schema
    tm = typeMap s
    sm = primSizeMap `M.union` fmap typeSize tm
    lu k = fromMaybe
            (error $ "typeSizeMap: key '" ++ (T.unpack . unIdentifier) k ++ "' not in map")
            (k `M.lookup` sm)
    typeSize t@(Type _ d) =
      case d of
        Synonym r       -> lu r
        Range _ _       -> tagToSize (tagForType t)
        Array r l       -> let sr = lu r
                               smin = sizeMin sr
                               smax = sizeMax sr
                               l' = fromIntegral l
                           in mkSize (l' * smin) (l' * smax)
        Vector r l      -> let sr = lu r
                               l' = fromIntegral l
                               ts = sizeMax $ tagToSize (tagForType t)
                               smax = sizeMax sr
                           in mkSize ts (ts + (l' * smax))
        Enumeration _   -> tagToSize $ tagForType t
        Record fs       -> let fsizes = map fieldSize fs
                               (mins,maxes) = unzip fsizes
                           in mkSize (sum mins) (sum maxes)
        Combination fs  -> let ts = sizeMax $ tagToSize $ tagForType t
                               fsizes = map fieldSize fs
                               (_,maxes) = unzip fsizes
                           in mkSize ts (ts + sum maxes)
        Union fs        -> let ts = sizeMax $ tagToSize $ tagForType t
                               fsizes = map fieldSize fs
                               (mins,maxes) = unzip fsizes
                           in mkSize (ts + minimum mins) (ts + maximum maxes)

    -- In theory, empty fields shouldn't go in Records ever. But just in case
    -- we change our minds, let's handle that case any way.
    fieldSize (EmptyField _) = (0,0)
    fieldSize (DataField _ r) = let sz = lu r
                                in (sizeMin sz, sizeMax sz)

typeDepthMap :: IsSchema a => a -> M.Map Identifier Integer
typeDepthMap schema = dm
  where
    s = getSchema schema
    tm = typeMap s
    dm = primDepthMap `M.union` fmap typeDepth tm
    lu n = fromJust $ n `M.lookup` dm

    typeDepth (Type _ d) =
      let rdepth =
            case d of
              Synonym r      -> lu r
              Range _ _      -> 0
              Array r _      -> lu r
              Vector r _     -> lu r
              Enumeration _  -> 0
              Record fs      -> maximum $ map fieldDepth fs
              Combination fs -> maximum $ map fieldDepth fs
              Union fs       -> maximum $ map fieldDepth fs
      in 1 + rdepth

    fieldDepth (EmptyField _) = 0
    fieldDepth (DataField _ r) = lu r

tagForType :: Type -> Tag
tagForType (Type _ d) =
  case d of
    Synonym _       -> error "No tag for synonyms."
    Array _ _       -> error "No tag for array."
    Record _        -> error "No tag for records."
    Range _ l       -> tagRequired l
    Vector _ l      -> tagRequired l
    Combination fs  -> tagForBits (length fs)

    -- For enumerations and unions, we add one to the length because
    -- we do not allow tags for these two prototypes with a value of
    -- 0. This is to avoid a common class of errors where a
    -- default-initialized struct/enum in C is 0. This helps catch
    -- cases where the user forgot to initialize the tag.
    Enumeration vs  -> tagRequired (length vs + 1)
    Union fs        -> tagRequired (length fs + 1)


primHashMap :: M.Map Identifier (T.Text,Hash)
primHashMap = M.fromList (zip allPrimNames vs)
  where
    ns = map unIdentifier allPrimNames
    hs = map mkHash ns
    vs = zip ns hs

primSizeMap :: M.Map Identifier Size
primSizeMap = M.fromList $ zip ns sz
  where
    ns = map primToText allPrims
    sz = map primToSize allPrims

primDepthMap :: M.Map Identifier Integer
primDepthMap = M.fromList $ zip ns ds
  where
    ns = map primToText allPrims
    ds = repeat 1

showNumSigned :: (Ord a, Show a, Num a) => a -> T.Text
showNumSigned v = let v' = abs v
                      v'' = T.pack . show $ v'
                  in if v < 0
                       then '-' `T.cons` v''
                       else '+' `T.cons` v''
