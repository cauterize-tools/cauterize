{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Schema.Arbitrary
 ( genTypeRuns
 , arbSchemaParam
 , ProtoParam(..)
 ) where

import Test.QuickCheck.Gen
import Control.Monad
import Data.Word
import qualified Data.Set as S

import Cauterize.Common.Types
import Cauterize.Schema.Types

data ProtoParam = ParamSynonym
                | ParamArray
                | ParamVector
                | ParamStruct
                | ParamSet
                | ParamEnum
  deriving (Show, Eq, Ord)

maxFields, maxRunTypes :: (Num a) => a
maxRunTypes = 3
maxFields = 9

arbArraySize :: Gen Integer
arbArraySize = choose (1,384)

arbSchemaParam :: S.Set ProtoParam -> Int -> Gen Schema
arbSchemaParam ps typeCount =
  liftM3 Schema (elements schemaNames)
                (elements schemaNames)
                genTypesParam
  where
    genTypesParam = liftM (biTys ++) $ go typeCount genNames (map show bis)
    someArbs = map protoToArb (S.toList ps)
    go 0 _ _ = return []
    go _ [] _ = return []
    go runsLeft (n:names) tyUniverse = do
      let arbs' = sequence (sequence someArbs tyUniverse) n
      t <- oneof arbs'
      ts <- go (runsLeft - 1) names (n:tyUniverse)
      return (t:ts)

genTypeRuns :: Word -> Gen [ScType]
genTypeRuns runs = liftM (biTys ++) $ go runs genNames (map show bis)
  where
    go 0 _ _ = return []
    go runs' ns ex = do
      tyCount <- elements [1..maxRunTypes]
      let (nowNames,laterNames) = splitAt tyCount ns
      ts <- genTypes nowNames ex
      rs <- go (runs' - 1) laterNames (ex ++ nowNames)
      return $ ts ++ rs

genTypes :: [Name] -> [Name] -> Gen [ScType]
genTypes [] _ = return []
genTypes (thisName:restNames) ex = do
  let arbs' = sequence (sequence arbs ex) thisName
  t <- oneof arbs'
  r <- genTypes restNames ex
  return $ t:r

protoToArb :: ProtoParam -> ([Name] -> Name -> Gen ScType)
protoToArb ParamSynonym = arbSynonym
protoToArb ParamArray = arbArray
protoToArb ParamVector = arbVector
protoToArb ParamStruct = arbStruct
protoToArb ParamSet = arbSet
protoToArb ParamEnum = arbEnum

arbs :: [[Name] -> Name -> Gen ScType]
arbs = [ arbSynonym
       , arbArray
       , arbVector
       , arbStruct
       , arbSet
       , arbEnum
       ]

arbSynonym :: [Name] -> Name -> Gen ScType
arbSynonym _ n = liftM (Synonym . TSynonym n) arbBi

arbArray :: [Name] -> Name -> Gen ScType
arbArray ts n = liftM2 (\t s -> Array $ TArray n t s) (elements ts) arbArraySize

arbVector :: [Name] -> Name -> Gen ScType
arbVector ts n = liftM2 (\t s -> Vector $ TVector n t s) (elements ts) arbArraySize

arbStruct :: [Name] -> Name -> Gen ScType
arbStruct ts n = arbFielded arbContainerField ts n (\n' fs -> Struct $ TStruct n' fs)

arbSet ::  [Name] -> Name -> Gen ScType
arbSet ts n = arbFielded arbField ts n (\n' fs -> Set $ TSet n' fs)

arbEnum ::  [Name] -> Name -> Gen ScType
arbEnum ts n = arbFielded arbField ts n (\n' fs -> Enum $ TEnum n' fs)

arbFielded :: ([Name] -> Name -> Gen (Integer -> Field))
           -> [Name] -> Name -> (Name -> Fields -> b) -> Gen b
arbFielded gen ts n cstr = do
  fieldCount <- elements [1..maxFields] :: Gen Integer
  let fieldNames = take (fromIntegral fieldCount) genNames
  let arbIRef' = gen ts
  fieldFs <- mapM arbIRef' fieldNames
  let fieldFs' = zipWith ($) fieldFs [0..(fieldCount - 1)]

  return $ cstr n (Fields fieldFs')

arbField :: [Name] -> Name -> Gen (Integer -> Field)
arbField ts n = frequency [(3, arbContainerField ts n), (1, arbEmptyField n)]

arbContainerField :: [Name] -> Name -> Gen (Integer -> Field)
arbContainerField ts n = liftM (Field n) $ elements ts

arbEmptyField :: Name -> Gen (Integer -> Field)
arbEmptyField n = return $ EmptyField n

sequences :: [a] -> [[a]]
sequences ls = ls' ++ [i ++ [a] | i <- sequences ls, a <- ls]
  where
    ls' = map (:[]) ls

schemaNames :: [Name]
schemaNames = take 100 genNames

genNames :: [Name]
genNames = let syms = ["a","e","i","o","u","y"]
           in map concat $ sequences syms

bis :: [BuiltIn]
bis = [minBound..maxBound]

biTys :: [ScType]
biTys = map (BuiltIn . TBuiltIn) bis

arbBi :: Gen BuiltIn
arbBi = elements bis
