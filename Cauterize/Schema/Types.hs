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
  ) where

import Cauterize.Common.BuiltIn
import Data.List
import Data.Maybe

import Data.Graph

import qualified Data.Map as M

type Name = String
type Signature = String
type Cycle = [Name]

data Schema t a = Schema
  { schemaName :: String
  , schemaVersion :: String
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

data IndexedRef t = IndexedRef Name t Integer
  deriving (Show, Ord, Eq)

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
refSig sm (IndexedRef n m _) = concat ["(field ", n, " ", luSig m, ")"]
  where
    luSig na = fromJust $ na `M.lookup` sm

-- | Creates a map of Type Names to Type Signatures
schemaSigMap :: (Ord a) => Schema Name a -> M.Map Name Signature
schemaSigMap schema = resultMap
  where
    tyMap = schemaTypeMap schema
    resultMap = fmap (typeSig resultMap) tyMap

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

referredNames :: Type Name a -> [Name]
referredNames (TBuiltIn _ _) = []
referredNames (TScalar _ b _) = [show b]
referredNames (TConst _ b _ _) = [show b]
referredNames (TFixedArray _ n _ _) = [n]
referredNames (TBoundedArray _ n _ _) = [n]
referredNames (TStruct _ fs _) = nub $  map (\(IndexedRef _ n _) -> n) fs
referredNames (TSet _ fs _) = nub $  map (\(IndexedRef _ n _) -> n) fs
referredNames (TEnum _ vs _) = nub $ map (\(IndexedRef _ n _) -> n) vs
referredNames (TPartial _ fs _) = nub $ map (\(IndexedRef _ n _) -> n) fs
referredNames (TPad _ _ _) = []

data SchemaErrors = DuplicateNames [Name]
                  | Cycles [Cycle]
  deriving (Show)

checkSchema :: Schema Name a -> [SchemaErrors]
checkSchema s@(Schema _ _ fs) = catMaybes [duplicateNames, cycles]
  where
    duplicateNames = case duplicates $ map (\(FType t) -> typeName t) fs of
                        [] -> Nothing
                        ds -> Just $ DuplicateNames ds
    cycles = case schemaCycles s of
                [] -> Nothing
                cs -> Just $ Cycles cs


duplicates :: (Eq a, Ord a) => [a] -> [a]
duplicates ins = map fst $ M.toList dups
  where
    dups = M.filter (>1) counts
    counts = foldl insertWith M.empty ins
    insertWith m x = M.insertWith ((+) :: (Int -> Int -> Int)) x 1 m
  
padShowInteger :: Integer -> String
padShowInteger v = let w = 20
                       v' = abs v
                       v'' = show v'
                       num = replicate (w - length v'') '0' ++ v''
                   in if v < 0
                        then '-':num
                        else '+':num

{-
import Text.PrettyPrint
import Text.PrettyPrint.Class

instance Pretty (Schema t a) where
  pretty (Schema n v fs) = parens $ text "schema" <+> (doubleQuotes . text) n <+> (doubleQuotes . text) v <+> pfs
    where
      pfs = vcat $ map pretty fs

instance Pretty (SchemaForm t a) where
  pretty (FType t) = pretty t

instance Pretty (Type t a) where
  pretty (TBuiltIn _ _) = empty
  pretty (TScalar n b _) = parens $ text "scalar" <+> text n <+> (text . show) b
  pretty (TConst n b i _) = parens $ text "const" <+> text n <+> (text . show) b <+> (text . show) i
  pretty (TFixedArray n m i _) = parens $ text "fixed" <+> text n <+> text m <+> (text . show) i
  pretty (TBoundedArray n m i _) = parens $ text "bounded" <+> text n <+> text m <+> (text . show) i
  pretty (TStruct n sfs _) = parens $ text "struct" <+> text n <+> psfs
    where
      psfs = vcat $ map pretty sfs
  pretty (TSet n sfs _) = parens $ text "set" <+> text n <+> psfs
    where
      psfs = vcat $ map pretty sfs
  pretty (TEnum n evs _) = parens $ text "enum" <+> text n <+> pevs
    where
      pevs = vcat $ map pretty evs
  pretty (TPartial n pfs _) = parens $ text "partial" <+> text n <+> ppfs
    where
      ppfs = vcat $ map pretty pfs
  pretty (TPad n i _) = parens $ text "pad" <+> text n <+> (text . show) i


instance Pretty (IndexedRef t) where
  pretty (IndexedRef n m _) = parens $ text "field" <+> text n <+> text m
-}
