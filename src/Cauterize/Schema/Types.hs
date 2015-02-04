{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Schema.Types
  ( Cycle
  , Schema(..)
  , ScType(..)

  , schemaTypeMap
  , checkSchema
  , typeName
  , referredNames

  , prettyPrint
  ) where

import Cauterize.Common.Types

import Data.Maybe

import Data.Graph

import qualified Data.Set as L
import qualified Data.Map as M

import Text.PrettyPrint
import Text.PrettyPrint.Class

type Cycle = [Name]

data Schema = Schema Name Version [ScType]
  deriving (Show)

data ScType = BuiltIn TBuiltIn
            | Scalar  TScalar
            | Array   TArray
            | Vector  TVector
            | Struct  TStruct
            | Set     TSet
            | Enum    TEnum
  deriving (Show, Ord, Eq)

schemaTypeMap :: Schema -> M.Map Name ScType
schemaTypeMap (Schema _ _ fs) = M.fromList $ map (\t -> (typeName t, t)) fs

typeName :: ScType -> Name
typeName (BuiltIn (TBuiltIn b)) = show b
typeName (Scalar (TScalar n _)) = n
typeName (Array (TArray n _ _)) = n
typeName (Vector (TVector n _ _)) = n
typeName (Struct (TStruct n _)) = n
typeName (Set (TSet n _)) = n
typeName (Enum (TEnum n _)) = n

referredNames :: ScType -> [Name]
referredNames (BuiltIn t) = referencesOf t
referredNames (Scalar t) = referencesOf t
referredNames (Array t) = referencesOf t
referredNames (Vector t) = referencesOf t
referredNames (Struct t) = referencesOf t
referredNames (Set t) = referencesOf t
referredNames (Enum t) = referencesOf t

data SchemaErrors = DuplicateNames [Name]
                  | Cycles [Cycle]
                  | NonExistent [Name]
  deriving (Show)

-- |If checkSchema returns [], then the Schema should be safe to operate on
-- with any of the methods provided in the Cauterize.Schema module.
checkSchema :: Schema -> [SchemaErrors]
checkSchema s@(Schema _ _ ts) = catMaybes [duplicateNames, cycles, nonExistent]
  where
    tns  = map typeName ts
    duplicateNames = case duplicates tns of
                        [] -> Nothing
                        ds -> Just $ DuplicateNames ds
    cycles = case schemaCycles s of
                [] -> Nothing
                cs -> Just $ Cycles cs
    nonExistent = let rSet = L.fromList $ concatMap referredNames ts
                      tnSet = L.fromList tns
                  in case L.toList $ rSet `L.difference` tnSet of
                      [] -> Nothing
                      bn -> Just $ NonExistent bn

schemaCycles :: Schema -> [Cycle]
schemaCycles s = typeCycles (map snd $ M.toList tyMap)
  where
    tyMap = schemaTypeMap s

    typeCycles :: [ScType] -> [Cycle]
    typeCycles ts = let ns = map (\t -> (typeName t, typeName t, referredNames t)) ts
                    in mapMaybe isScc (stronglyConnComp ns)
      where
        isScc (CyclicSCC vs) = Just vs
        isScc _ = Nothing

duplicates :: (Eq a, Ord a) => [a] -> [a]
duplicates ins = map fst $ M.toList dups
  where
    dups = M.filter (>1) counts
    counts = foldl insertWith M.empty ins
    insertWith m x = M.insertWith ((+) :: (Int -> Int -> Int)) x 1 m

-- Instances

prettyPrint :: Schema -> String
prettyPrint = show . pretty

pShow :: (Show a) => a -> Doc
pShow = text . show

instance Pretty Schema where
  pretty (Schema n v fs) = parens $ hang ps 1 pfs
    where
      ps = text "schema" <+> text n <+> text v
      pfs = vcat $ map pretty fs

instance Pretty ScType where
  pretty (BuiltIn _) = empty
  pretty (Scalar (TScalar n b)) = parens $ text "scalar" <+> text n <+> pShow b
  pretty (Array (TArray n m s)) = parens $ text "array" <+> text n <+> text m <+> integer s
  pretty (Vector (TVector n m s)) = parens $ text "vector" <+> text n <+> text m <+> integer s
  pretty (Struct (TStruct n fs)) = prettyFielded "struct" n fs
  pretty (Set (TSet n fs)) = prettyFielded "set" n fs
  pretty (Enum (TEnum n fs)) = prettyFielded "enum" n fs

prettyFielded :: String -> Name -> Fields -> Doc
prettyFielded t n fs = parens $ hang pt 1 pfs
  where
    pt = text t <+> text n
    pfs = schemaPrettyFields fs

schemaPrettyFields :: Fields -> Doc
schemaPrettyFields (Fields fs) = parens $ hang (text "fields") 1 pfs
  where
    pfs = vcat $ map schemaPrettyRefs fs

schemaPrettyRefs :: Field -> Doc
schemaPrettyRefs  (EmptyField n _) = parens $ text "field" <+> text n
schemaPrettyRefs  (Field n m _) = parens $ text "field" <+> text n <+> text m
