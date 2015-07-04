{-# LANGUAGE StandaloneDeriving #-}
module Cauterize.Schema.CheckerNew
  ( CheckedSchema
  , checkSchema
  ) where

import Data.List (intersect, group, sort)
import Data.Maybe
import Data.Graph
import Debug.Trace

import Cauterize.Schema.TypesNew
import Cauterize.Schema.UtilNew

data CheckedSchema = CheckedSchema Schema
  deriving (Show)

instance IsSchema CheckedSchema where
  getSchema (CheckedSchema s) = s

{-
 - Checks to perform:
 -
 -    * duplicate names
 -    * reuse of primitive names
 -    * cycles
 -    * reference to non-existent types
 -    ! duplicate field names
 -    ! duplicate enumeration values
 -    ! negative sizes
 -    ! inexpressible range type
 -}


checkSchema :: Schema -> Either String CheckedSchema
checkSchema s =
  case errs of
    [] -> Right (CheckedSchema s)
    _ -> Left (unlines errs)
  where
    errs = mapMaybe ($ s) checkers
    checkers =
      [ checkDuplicateNames
      , checkReusePrimitiveNames
      , checkCycles
      , checkReferenceToNonExistentType
      , checkForDuplicateFieldNames
      ]

checkDuplicateNames :: Schema -> Maybe String
checkDuplicateNames s =
  case duplicates (schemaTypeNames s) of
    [] -> Nothing
    ds -> Just ("Duplicate type names found: " ++ show ds)

checkReusePrimitiveNames :: Schema -> Maybe String
checkReusePrimitiveNames s =
  case schemaTypeNames s `intersect` allPrimNames of
    [] -> Nothing
    ns -> Just ("Reuse of primitive names found: " ++ show ns)

checkCycles :: Schema -> Maybe String
checkCycles s =
  case mapMaybe isScc nodesscc of
    [] -> Nothing
    cs -> Just ("Found referential cycles: " ++ show cs)
  where
    nodes = map (\t -> (typeName t, typeName t, typeReferences t)) (schemaTypes s)
    nodesscc = stronglyConnComp nodes

    isScc (CyclicSCC vs) = Just vs
    isScc _ = Nothing

checkReferenceToNonExistentType :: Schema -> Maybe String
checkReferenceToNonExistentType s =
  case mapMaybe checkType (schemaTypes s) of
    [] -> Nothing
    es -> Just ("Found invalid references: " ++ show es)
  where
    ts = schemaTypeNames s ++ allPrimNames
    checkType t =
      let rs = typeReferences t
      in case mapMaybe (\r -> if r `elem` ts then Nothing else Just r) rs of
          [] -> Nothing
          x -> Just (typeName t, x)

checkForDuplicateFieldNames :: Schema -> Maybe String
checkForDuplicateFieldNames s =
  case mapMaybe checkType (schemaTypes s) of
    [] -> Nothing
    es -> Just ("Found duplicate field names: " ++ show es)
  where
    checkFields n fs =
      let ns = map fieldName fs
      in case duplicates ns of
            [] -> Nothing
            ds -> Just (n, ds)

    checkType (Type n (Record fs)) = checkFields n fs
    checkType (Type n (Combination fs)) = checkFields n fs
    checkType (Type n (Union fs)) = checkFields n fs
    checkType _ = Nothing

duplicates :: Ord b => [b] -> [b]
duplicates ls = map (!! 1) $ filter (\l -> 1 < length l) $ group . sort $ ls
