{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Schema.UtilNew
  ( schemaTypeNames
  , typeReferences
  , typeMap
  , typeHashMap
  , typeSizeMap
  , tagForType
  ) where

import Cauterize.Schema.TypesNew
import Cauterize.HashNew
import Cauterize.CommonTypesNew
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
              Array r l       -> fmt "array" [n',unIdentifier r,showNumSigned l]
              Vector r l      -> fmt "vector" [n',unIdentifier r,showNumSigned l]
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

tagForType :: Type -> Tag
tagForType (Type _ d) =
  case d of
    Synonym _       -> error "No tag for synonyms."
    Array _ _       -> error "No tag for array."
    Record _        -> error "No tag for records."
    Range _ l       -> tagRequired l
    Vector _ l      -> tagRequired l
    Enumeration vs  -> tagRequired (length vs)
    Combination fs  -> tagForBits (length fs)
    Union fs        -> tagRequired (length fs)


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

showNumSigned :: (Ord a, Show a, Num a) => a -> T.Text
showNumSigned v = let v' = abs v
                      v'' = T.pack . show $ v'
                  in if v < 0
                       then '-' `T.cons` v''
                       else '+' `T.cons` v''

tagRequired :: Integral a => a -> Tag
tagRequired i | (0          <= i') && (i' < 256) = T1
              | (256        <= i') && (i' < 65536) = T2
              | (25536      <= i') && (i' < 4294967296) = T4
              | (4294967296 <= i') && (i' <= 18446744073709551615) = T8
              | otherwise = error $ "Cannot express tag for value: " ++ show i'
  where
    i' = fromIntegral i :: Integer

tagForBits :: Integral a => a -> Tag
tagForBits v | 0 <= v' && v' <= 8 = T1
             | 0 <= v' && v' <= 16 = T2
             | 0 <= v' && v' <= 32 = T4
             | 0 <= v' && v' <= 64 = T8
             | otherwise = error
                 $ "Cannot express '" ++ show v' ++ "' bits in a bitfield."
  where
    v' = fromIntegral v :: Integer
