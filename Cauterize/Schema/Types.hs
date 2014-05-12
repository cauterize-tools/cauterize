module Cauterize.Schema.Types
  ( Cycle
  , Schema(..)
  , SchemaForm(..)
  , ScType(..)

  , schemaTypeMap
  , schemaSigMap
  , checkSchema
  , typeName
  ) where

import Cauterize.Common.Primitives
import Cauterize.Common.IndexedRef
import Cauterize.Common.References

import Cauterize.Common.Types.BuiltIn
import Cauterize.Common.Types.Scalar
import Cauterize.Common.Types.Const
import Cauterize.Common.Types.FixedArray
import Cauterize.Common.Types.BoundedArray
import Cauterize.Common.Types.Struct
import Cauterize.Common.Types.Set
import Cauterize.Common.Types.Enum
import Cauterize.Common.Types.Partial
import Cauterize.Common.Types.Pad

import Data.Maybe

import Data.Graph

import qualified Data.Set as L
import qualified Data.Map as M

type Cycle = [Name]

data Schema t = Schema
  { schemaName :: Name
  , schemaVersion :: Version
  , schemaForms :: [SchemaForm t]
  }
  deriving (Show)


data SchemaForm t = FType (ScType t)
  deriving (Show)

data ScType t = ScBuiltIn      TBuiltIn
              | ScScalar       TScalar
              | ScConst        TConst
              | ScFixedArray   (TFixedArray t)
              | ScBoundedArray (TBoundedArray t)
              | ScStruct       (TStruct t)
              | ScSet          (TSet t)
              | ScEnum         (TEnum t)
              | ScPartial      (TPartial t)
              | ScPad          TPad
  deriving (Show, Ord, Eq)

schemaTypeMap :: Schema t -> M.Map Name (ScType t)
schemaTypeMap (Schema _ _ fs) = M.fromList $ map (\(FType t) -> (typeName t, t)) fs

typeName :: ScType t -> Name
typeName (ScBuiltIn (TBuiltIn b)) = show b
typeName (ScScalar (TScalar n _)) = n
typeName (ScConst (TConst n _ _)) = n
typeName (ScFixedArray (TFixedArray n _ _)) = n
typeName (ScBoundedArray (TBoundedArray n _ _)) = n
typeName (ScStruct (TStruct n _)) = n
typeName (ScSet (TSet n _)) = n
typeName (ScEnum (TEnum n _)) = n
typeName (ScPartial (TPartial n _)) = n
typeName (ScPad (TPad n _)) = n

biSig :: BuiltIn -> Signature
biSig b = "(" ++ show b ++ ")"

typeSig :: M.Map Name Signature -> ScType Name -> Signature 
typeSig sm t =
  case t of
    (ScBuiltIn (TBuiltIn b)) -> biSig b
    (ScScalar (TScalar n b)) -> concat ["(scalar ", n, " ", biSig b, ")"]
    (ScConst (TConst n b i)) -> concat ["(const ", n, " ", biSig b, " ", padShowInteger i, ")"]
    (ScFixedArray (TFixedArray n m i)) -> concat ["(fixed ", n, " ", luSig m, " ", padShowInteger i, ")"]
    (ScBoundedArray (TBoundedArray n m i)) -> concat ["(bounded ", n, " ", luSig m, " ", padShowInteger i, ")"]
    (ScStruct (TStruct n rs)) -> concat ["(struct ", n, " ", unwords $ map (refSig sm) rs, ")"]
    (ScSet (TSet n rs)) -> concat ["(set ", n, " ", unwords $ map (refSig sm) rs, ")"]
    (ScEnum (TEnum n rs)) -> concat ["(enum ", n, " ", unwords $ map (refSig sm) rs, ")"]
    (ScPartial (TPartial n rs)) -> concat ["(partial ", n, " ", unwords $ map (refSig sm) rs, ")"]
    (ScPad (TPad n i)) -> concat ["(pad ", n, " ", padShowInteger i, ")"]
  where
    luSig n = fromJust $ n `M.lookup` sm

-- | Creates a map of Type Names to Type Signatures
schemaSigMap :: Schema Name -> M.Map Name Signature
schemaSigMap schema = resultMap
  where
    tyMap = schemaTypeMap schema
    resultMap = fmap (typeSig resultMap) tyMap

referredNames :: ScType Name -> [Name]
referredNames (ScBuiltIn t) = referencesOf t
referredNames (ScScalar t) = referencesOf t
referredNames (ScConst t) = referencesOf t
referredNames (ScFixedArray t) = referencesOf t
referredNames (ScBoundedArray t) = referencesOf t
referredNames (ScStruct t) = referencesOf t
referredNames (ScSet t) = referencesOf t
referredNames (ScEnum t) = referencesOf t
referredNames (ScPartial t) = referencesOf t
referredNames (ScPad t) = referencesOf t

data SchemaErrors = DuplicateNames [Name]
                  | Cycles [Cycle]
                  | NonExistent [Name]
  deriving (Show)

-- |If checkSchema returns [], then the Schema should be safe to operate on
-- with any of the methods provided in the Cauterize.Schema module.
checkSchema :: Schema Name -> [SchemaErrors]
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
