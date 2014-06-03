{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Schema.Types
  ( Cycle
  , Schema(..)
  , ScType(..)

  , schemaTypeMap
  , schemaSigMap
  , checkSchema
  , typeName
  , referredNames

  , prettyPrint
  ) where

import Cauterize.Common.Primitives
import Cauterize.Common.IndexedRef
import Cauterize.Common.References
import Cauterize.Common.Types

import Data.Maybe

import Data.Graph

import qualified Data.Set as L
import qualified Data.Map as M

import Text.PrettyPrint
import Text.PrettyPrint.Class

type Cycle = [Name]

data Schema t = Schema Name Version [ScType t]
  deriving (Show)

data ScType t = BuiltIn      TBuiltIn
              | Scalar       TScalar
              | Const        TConst
              | FixedArray   (TFixedArray t)
              | BoundedArray (TBoundedArray t)
              | Struct       (TStruct t)
              | Set          (TSet t)
              | Enum         (TEnum t)
              | Partial      (TPartial t)
              | Pad          TPad
  deriving (Show, Ord, Eq)

schemaTypeMap :: Schema t -> M.Map Name (ScType t)
schemaTypeMap (Schema _ _ fs) = M.fromList $ map (\t -> (typeName t, t)) fs

typeName :: ScType t -> Name
typeName (BuiltIn (TBuiltIn b)) = show b
typeName (Scalar (TScalar n _)) = n
typeName (Const (TConst n _ _)) = n
typeName (FixedArray (TFixedArray n _ _)) = n
typeName (BoundedArray (TBoundedArray n _ _)) = n
typeName (Struct (TStruct n _)) = n
typeName (Set (TSet n _)) = n
typeName (Enum (TEnum n _)) = n
typeName (Partial (TPartial n _)) = n
typeName (Pad (TPad n _)) = n

biSig :: BuiltIn -> Signature
biSig b = "(" ++ show b ++ ")"

typeSig :: M.Map Name Signature -> ScType Name -> Signature 
typeSig sm t =
  case t of
    (BuiltIn (TBuiltIn b)) -> biSig b
    (Scalar (TScalar n b)) -> concat ["(scalar ", n, " ", biSig b, ")"]
    (Const (TConst n b i)) -> concat ["(const ", n, " ", biSig b, " ", padShowInteger i, ")"]
    (FixedArray (TFixedArray n m i)) -> concat ["(fixed ", n, " ", luSig m, " ", padShowInteger i, ")"]
    (BoundedArray (TBoundedArray n m i)) -> concat ["(bounded ", n, " ", luSig m, " ", padShowInteger i, ")"]
    (Struct (TStruct n rs)) -> concat ["(struct ", n, " ", unwords $ map (refSig sm) (unFields rs), ")"]
    (Set (TSet n rs)) -> concat ["(set ", n, " ", unwords $ map (refSig sm) (unFields rs), ")"]
    (Enum (TEnum n rs)) -> concat ["(enum ", n, " ", unwords $ map (refSig sm) (unFields rs), ")"]
    (Partial (TPartial n rs)) -> concat ["(partial ", n, " ", unwords $ map (refSig sm) (unFields rs), ")"]
    (Pad (TPad n i)) -> concat ["(pad ", n, " ", padShowInteger i, ")"]
  where
    luSig n = fromJust $ n `M.lookup` sm

-- | Creates a map of Type Names to Type Signatures
schemaSigMap :: Schema Name -> M.Map Name Signature
schemaSigMap schema = resultMap
  where
    tyMap = schemaTypeMap schema
    resultMap = fmap (typeSig resultMap) tyMap

referredNames :: ScType Name -> [Name]
referredNames (BuiltIn t) = referencesOf t
referredNames (Scalar t) = referencesOf t
referredNames (Const t) = referencesOf t
referredNames (FixedArray t) = referencesOf t
referredNames (BoundedArray t) = referencesOf t
referredNames (Struct t) = referencesOf t
referredNames (Set t) = referencesOf t
referredNames (Enum t) = referencesOf t
referredNames (Partial t) = referencesOf t
referredNames (Pad t) = referencesOf t

data SchemaErrors = DuplicateNames [Name]
                  | Cycles [Cycle]
                  | NonExistent [Name]
  deriving (Show)

-- |If checkSchema returns [], then the Schema should be safe to operate on
-- with any of the methods provided in the Cauterize.Schema module.
checkSchema :: Schema Name -> [SchemaErrors]
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

schemaCycles :: Schema Name -> [Cycle]
schemaCycles s = typeCycles (map snd $ M.toList tyMap)
  where
    tyMap = schemaTypeMap s

    typeCycles :: [ScType Name] -> [Cycle]
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
  
padShowInteger :: Integer -> String
padShowInteger v = let v' = abs v
                       v'' = show v'
                   in if v < 0
                        then '-':v''
                        else '+':v''

-- Instances

prettyPrint :: Schema String -> String
prettyPrint = show . pretty

pShow :: (Show a) => a -> Doc
pShow = text . show 

instance Pretty (Schema String) where
  pretty (Schema n v fs) = parens $ hang ps 1 pfs
    where
      ps = text "schema" <+> (doubleQuotes . text) n <+> (doubleQuotes . text) v
      pfs = vcat $ map pretty fs

instance Pretty (ScType String) where
  pretty (BuiltIn _) = empty
  pretty (Scalar (TScalar n b)) = parens $ text "scalar" <+> text n <+> pShow b
  pretty (Const (TConst n b i)) = parens $ text "const" <+> text n <+> pShow b <+> integer i
  pretty (FixedArray (TFixedArray n m s)) = parens $ text "fixed" <+> text n <+> text m <+> integer s
  pretty (BoundedArray (TBoundedArray n m s)) = parens $ text "bounded" <+> text n <+> text m <+> integer s
  pretty (Struct (TStruct n fs)) = prettyFielded "struct" n fs
  pretty (Set (TSet n fs)) = prettyFielded "set" n fs
  pretty (Enum (TEnum n fs)) = prettyFielded "enum" n fs
  pretty (Partial (TPartial n fs)) = prettyFielded "partial" n fs
  pretty (Pad (TPad n i)) = parens $ text "pad" <+> text n <+> integer i

prettyFielded :: String -> Name -> Fields Name -> Doc
prettyFielded t n fs = parens $ hang pt 1 pfs
  where
    pt = text t <+> text n
    pfs = schemaPrettyFields fs
