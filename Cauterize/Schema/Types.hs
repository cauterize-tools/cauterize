{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Cauterize.Schema.Types
  ( Name
  , Signature
  , Cycle
  , Schema(..)
  , SchemaForm(..)
  , Type(..)
  , IndexedRef(..)

  , schemaTypeMap
  , schemaSigMap
  , checkSchema
  , annotateWith
  , typeName
  ) where

import Cauterize.Common.BuiltIn
import Data.List
import Data.Maybe

import Data.Graph

import qualified Data.Set as L
import qualified Data.Map as M

type Name = String
type Version = String
type Signature = String
type Cycle = [Name]

data Schema t a = Schema
  { schemaName :: Name
  , schemaVersion :: Version
  , schemaForms :: [SchemaForm t a]
  }
  deriving (Show)


data SchemaForm t a = FType (Type t a)
  deriving (Show)

data Type t a = TBuiltIn BuiltIn a
              | TScalar Name BuiltIn a
              | TConst Name BuiltIn Integer a

              | TFixedArray Name t Integer a
              | TBoundedArray Name t Integer a

              | TStruct Name [IndexedRef t] a
              | TSet Name [IndexedRef t] a

              | TEnum Name [IndexedRef t] a
              | TPartial Name [IndexedRef t] a

              | TPad Name Integer a
  deriving (Show, Ord, Eq)

instance Functor (Type t) where
  fmap fn t = 
    case t of
      (TBuiltIn b a) -> TBuiltIn b (fn a)
      (TScalar n b a) -> TScalar n b (fn a)
      (TConst n b i a) -> TConst n b i (fn a)
      (TFixedArray n r i a) -> TFixedArray n r i (fn a)
      (TBoundedArray n r i a) -> TFixedArray n r i (fn a)
      (TStruct n rs a) -> TStruct n rs (fn a)
      (TSet n rs a) -> TSet n rs (fn a)
      (TEnum n rs a) -> TEnum n rs (fn a)
      (TPartial n rs a) -> TPartial n rs (fn a)
      (TPad n i a) -> TPad n i (fn a)

data IndexedRef r where
  NameRef :: Name -> Name -> Integer -> IndexedRef Name
  TypeRef :: Name -> Type t a -> Integer -> IndexedRef (Type t a)

deriving instance (Show r) => Show (IndexedRef r)
deriving instance (Ord r) => Ord (IndexedRef r)
deriving instance (Eq r) => Eq (IndexedRef r)

schemaTypeMap :: Schema t a -> M.Map Name (Type t a)
schemaTypeMap (Schema _ _ fs) = M.fromList $ map (\(FType t) -> (typeName t, t)) fs

typeName :: Type t a -> Name
typeName (TBuiltIn b _) = show b
typeName (TScalar n _ _) = n
typeName (TConst n _ _ _) = n
typeName (TFixedArray n _ _ _) = n
typeName (TBoundedArray n _ _ _) = n
typeName (TStruct n _ _) = n
typeName (TSet n _ _) = n
typeName (TEnum n _ _) = n
typeName (TPartial n _ _) = n
typeName (TPad n _ _) = n

annotateWith :: Type t a -> (Type t a -> b) -> Type t b
annotateWith t fn =
  case t of
    (TBuiltIn b _) -> TBuiltIn b (fn t)
    (TScalar n b _) -> TScalar n b (fn t)
    (TConst n b i _) -> TConst n b i (fn t)
    (TFixedArray n r i _) -> TFixedArray n r i (fn t)
    (TBoundedArray n r i _) -> TFixedArray n r i (fn t)
    (TStruct n rs _) -> TStruct n rs (fn t)
    (TSet n rs _) -> TSet n rs (fn t)
    (TEnum n rs _) -> TEnum n rs (fn t)
    (TPartial n rs _) -> TPartial n rs (fn t)
    (TPad n i _) -> TPad n i (fn t)

biSig :: BuiltIn -> Signature
biSig b = "(" ++ show b ++ ")"

typeSig :: (Ord a) => M.Map Name Signature -> Type Name a -> Signature 
typeSig sm t =
  case t of
    (TBuiltIn b _) -> biSig b
    (TScalar n b _) -> concat ["(scalar ", n, " ", biSig b, ")"]
    (TConst n b i _) -> concat ["(const ", n, " ", biSig b, " ", padShowInteger i, ")"]
    (TFixedArray n m i _) -> concat ["(fixed ", n, " ", luSig m, " ", padShowInteger i, ")"]
    (TBoundedArray n m i _) -> concat ["(bounded ", n, " ", luSig m, " ", padShowInteger i, ")"]
    (TStruct n rs _) -> concat ["(struct ", n, " ", unwords $ map (refSig sm) rs, ")"]
    (TSet n rs _) -> concat ["(set ", n, " ", unwords $ map (refSig sm) rs, ")"]
    (TEnum n rs _) -> concat ["(enum ", n, " ", unwords $ map (refSig sm) rs, ")"]
    (TPartial n rs _) -> concat ["(partial ", n, " ", unwords $ map (refSig sm) rs, ")"]
    (TPad n i _) -> concat ["(pad ", n, " ", padShowInteger i, ")"]
  where
    luSig n = fromJust $ n `M.lookup` sm

refSig :: M.Map Name Signature -> IndexedRef Name -> Signature
refSig sm (NameRef n m _) = concat ["(field ", n, " ", luSig m, ")"]
  where
    luSig na = fromJust $ na `M.lookup` sm

-- | Creates a map of Type Names to Type Signatures
schemaSigMap :: (Ord a) => Schema Name a -> M.Map Name Signature
schemaSigMap schema = resultMap
  where
    tyMap = schemaTypeMap schema
    resultMap = fmap (typeSig resultMap) tyMap

referredNames :: Type Name a -> [Name]
referredNames (TBuiltIn _ _) = []
referredNames (TScalar _ b _) = [show b]
referredNames (TConst _ b _ _) = [show b]
referredNames (TFixedArray _ n _ _) = [n]
referredNames (TBoundedArray _ n _ _) = [n]
referredNames (TStruct _ fs _) = nub $  map refRef fs
referredNames (TSet _ fs _) = nub $  map refRef fs
referredNames (TEnum _ vs _) = nub $ map refRef vs
referredNames (TPartial _ fs _) = nub $ map refRef fs
referredNames (TPad _ _ _) = []

refRef :: IndexedRef t -> t
refRef (NameRef _ n _) = n
refRef (TypeRef _ n _) = n

data SchemaErrors = DuplicateNames [Name]
                  | Cycles [Cycle]
                  | NonExistent [Name]
  deriving (Show)

-- |If checkSchema returns [], then the Schema should be safe to operate on
-- with any of the methods provided in the Cauterize.Schema module.
checkSchema :: Schema Name a -> [SchemaErrors]
checkSchema s@(Schema _ _ fs) = catMaybes [duplicateNames, cycles, nonExistent]
  where
    ts = map (\(FType t) -> t) fs
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

schemaCycles :: Schema Name a -> [Cycle]
schemaCycles s = typeCycles (map snd $ M.toList tyMap)
  where
    tyMap = schemaTypeMap s

    typeCycles :: [Type Name a] -> [Cycle]
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
