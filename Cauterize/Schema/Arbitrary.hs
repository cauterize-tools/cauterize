{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Schema.Arbitrary
 ( genTypeRuns
 , arbSchema
 , ValidSchema(unValidSchema)
 ) where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Control.Monad
import Data.Word

import Cauterize.Common.Field
import Cauterize.Common.Primitives
import Cauterize.Common.Types
import Cauterize.Schema.Types

newtype ValidSchema = ValidSchema { unValidSchema :: Schema }
  deriving (Show)

instance Arbitrary ValidSchema where
  arbitrary = liftM ValidSchema arbSchema

maxFields, maxRunTypes, maxRuns :: (Num a) => a
maxRuns = 5
maxRunTypes = 3
maxFields = 5

constRange :: (Num a, Enum a) => [a]
constRange = [1..10]

arbSchema :: Gen Schema
arbSchema = liftM3 Schema (elements schemaNames) (elements schemaNames) rs
  where
    rs = genTypeRuns maxRuns

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

arbs :: [[Name] -> Name -> Gen ScType]
arbs = [ arbScalar
       , arbConst
       , arbFixed
       , arbBounded
       , arbStruct
       , arbSet
       , arbEnum
       , arbPartial
       , arbPad
       ]

arbArraySize :: Gen Integer
arbArraySize = liftM fromIntegral (arbitrary :: Gen Word32)

arbScalar :: [Name] -> Name -> Gen ScType
arbScalar _ n = liftM (Scalar . TScalar n) arbBi

arbConst :: [Name] -> Name -> Gen ScType
arbConst _ n = liftM2 (\b i -> Const $ TConst n b i) arbBi (elements constRange)

arbFixed :: [Name] -> Name -> Gen ScType
arbFixed ts n = liftM2 (\t s -> FixedArray $ TFixedArray n t s) (elements ts) arbArraySize

arbBounded :: [Name] -> Name -> Gen ScType
arbBounded ts n = liftM2 (\t s -> BoundedArray $ TBoundedArray n t s) (elements ts) arbArraySize

arbStruct :: [Name] -> Name -> Gen ScType
arbStruct ts n = arbFielded ts n (\n' fs -> Struct $ TStruct n' fs)

arbSet ::  [Name] -> Name -> Gen ScType
arbSet ts n = arbFielded ts n (\n' fs -> Set $ TSet n' fs)

arbEnum ::  [Name] -> Name -> Gen ScType
arbEnum ts n = arbFielded ts n (\n' fs -> Enum $ TEnum n' fs)

arbPartial ::  [Name] -> Name -> Gen ScType
arbPartial ts n = arbFielded ts n (\n' fs -> Partial $ TPartial n' fs)

arbPad :: [Name] -> Name -> Gen ScType
arbPad _ n = liftM (Pad . TPad n) (elements [1..8])

arbFielded :: [Name] -> Name -> (Name -> Fields -> a) -> Gen a
arbFielded ts n cstr = do
  fieldCount <- elements [1..maxFields] :: Gen Integer
  let fieldNames = take (fromIntegral fieldCount) genNames
  let arbIRef' = arbIRef ts
  fieldFs <- mapM arbIRef' fieldNames
  let fieldFs' = zipWith ($) fieldFs [0..(fieldCount - 1)]

  return $ cstr n (Fields fieldFs')

arbIRef :: [Name] -> Name -> Gen (Integer -> Field)
arbIRef ts n = frequency [(3, arbField ts n), (1, arbEmptyField n)]

arbField :: [Name] -> Name -> Gen (Integer -> Field)
arbField ts n = liftM (Field n) $ elements ts

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
