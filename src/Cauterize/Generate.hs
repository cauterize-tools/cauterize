{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Generate
  ( generateSchema
  , generateSchemaWith

  , PrototypeVariant(..)

  , defaultMaximumTypes
  , defaultMaximumSize
  , defaultAllowedPrototypes
  , defaultMargin
  ) where

import Cauterize.CommonTypes
import Control.Monad
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Data.Word
import Data.Int
import qualified Cauterize.Schema as Schema
import qualified Cauterize.Specification as Spec
import qualified Data.Text as T

data PrototypeVariant
  = PVSynonym
  | PVRange
  | PVArray
  | PVVector
  | PVEnumeration
  | PVRecord
  | PVCombination
  | PVUnion
  deriving (Show, Eq, Ord, Enum, Bounded)

generateSchema :: IO Schema.Schema
generateSchema = generateSchemaWith defaultMaximumTypes defaultMaximumSize defaultMargin defaultAllowedPrototypes

generateSchemaWith :: Integer -> Integer -> Double -> [PrototypeVariant] -> IO Schema.Schema
generateSchemaWith maximumTypes maximumSize margin allowedPrototypes = generate $ generateSchemaWith' maximumTypes maximumSize margin allowedPrototypes

generateSchemaWith' :: Integer -> Integer -> Double -> [PrototypeVariant] -> Gen Schema.Schema
generateSchemaWith' maximumTypes maximumSize margin allowedPrototypes =
  liftM3 Schema.Schema (elements schemaNames)
                       (elements schemaNames)
                       (go maximumTypes maximumSize allNames [])
  where
    -- Returns the size of the schema
    size :: [Schema.Type] -> Integer
    size ts = let s = Spec.specSize . Spec.mkSpecification $ Schema.Schema "temp_schema" "0" ts
              in sizeMax s

    go :: Integer -> Integer -> [Identifier] -> [Schema.Type] -> Gen [Schema.Type]
    go _ _ [] _ = error "Ran out of names!"
    go tCount remSz (n:names) s
      | tCount <= 0 = return s
      | remSz <= 0 = return s
      | otherwise = do
          t <- genVariant s allowedPrototypes n

          let ts = t:s
              oldSz = size s
              sz = size (t:s)

          if sz <= maximumSize
            then go (tCount - 1) (maximumSize - sz) names ts
            else if let os = fromIntegral oldSz
                        ms = fromIntegral maximumSize
                    in (os / ms) > margin
              then return s -- if we're within 10% of the size, give up and return what we have
              else go tCount maximumSize (n:names) s

genVariant :: [Schema.Type] -> [PrototypeVariant] -> Identifier -> Gen Schema.Type
genVariant _ [] _ = error "Must specify at least one prototype variant."
genVariant existingTypes variants name = do
  v <- elements variants
  case v of
    PVSynonym -> genSynonym
    PVRange -> genRange
    PVArray -> genArray
    PVVector -> genVector
    PVEnumeration -> genEnumeration
    PVRecord -> genRecord
    PVCombination -> genCombination
    PVUnion -> genUnion
  where
    twrap = Schema.Type name
    existingNames = allPrimNames ++ map Schema.typeName existingTypes
    genSynonym = liftM (twrap . Schema.Synonym) (elements existingNames)
    genArray = liftM2 (\t l -> twrap (Schema.Array t (fromIntegral l))) (elements existingNames) arbLength
    genVector = liftM2 (\t l -> twrap (Schema.Vector t (fromIntegral l))) (elements existingNames) arbLength
    genRecord = liftM (twrap . Schema.Record) (genFieldsWithoutEmpty existingNames)
    genCombination = liftM (twrap . Schema.Combination) (genFields existingNames)
    genUnion = liftM (twrap . Schema.Union) (genFields existingNames)
    genEnumeration = liftM (twrap . Schema.Enumeration) genEnumValues
    genRange = do
      o <- arbitrary

      let op1 = fromIntegral o + 1 :: Integer
      let mbi = fromIntegral (maxBound :: Int64) :: Integer
      let mbw = fromIntegral (maxBound :: Word64) :: Integer

      r <- if o < 0
            then liftM fromIntegral (choose (op1, mbi))
            else liftM fromIntegral (choose (op1, mbw))

      return $ twrap (Schema.Range o r)

defaultMaximumTypes :: Integer
defaultMaximumTypes = 10

defaultMaximumSize :: Integer
defaultMaximumSize = 1000

defaultAllowedPrototypes :: [PrototypeVariant]
defaultAllowedPrototypes = [minBound..maxBound]

-- The margin to use as "close enough". Once our schema size is this proportion
-- of the maximum size, we can give up. This keeps things from running for an
-- absurdly long time as it seeks for an exact fit.
defaultMargin :: Double
defaultMargin = 0.9

-- A few functions for generating then names we'll use throughout this
-- generation process.
schemaNames :: [T.Text]
schemaNames = map unIdentifier $ take 100 allNames

allNames :: [Identifier]
allNames = let syms = ["a","e","i","o","u","y"]
           in map (unsafeMkIdentifier . concat) $ sequences syms

sequences :: [a] -> [[a]]
sequences ls = ls' ++ [i ++ [a] | i <- sequences ls, a <- ls]
  where
    ls' = map (:[]) ls

-- This helps pick a size for a lengthed type like an array or vector. It favors shorter sizes.
arbLength :: Gen Integer
arbLength = frequency [ (1000, choose (pow0,  pow8  - 1))
                      , (100, choose (pow8,  pow16 - 1))
                      , (10, choose (pow16, pow32 - 1))
                      , (1, choose (pow32, pow64 - 1))
                      ]
  where
    pow0  = (2 :: Integer)^(0  :: Integer)
    pow8  = (2 :: Integer)^(8  :: Integer)
    pow16 = (2 :: Integer)^(16 :: Integer)
    pow32 = (2 :: Integer)^(32 :: Integer)
    pow64 = (2 :: Integer)^(64 :: Integer)

-- This is used to build a set of fields for the types that use a field set.
genFields_ :: Gen (Identifier -> c) -> Gen [c]
genFields_ cstorGen = do
  count <- fieldCount
  fieldCstors <- replicateM count cstorGen
  let withNames = zipWith ($) fieldCstors allNames
  return withNames
  where
    fieldCount = let ixs = [1..64]
                     freqs = reverse ixs
                     gens = map return ixs
                 in frequency $ zip freqs gens

genFields :: [Identifier] -> Gen [Schema.Field]
genFields ts =
  genFields_ $ frequency [(1, liftM (flip Schema.DataField) (elements ts))
                         ,(1, return Schema.EmptyField)
                         ]

genEnumValues :: Gen [Identifier]
genEnumValues = sized (\s -> let s' | s <= 0 = 1
                                    | otherwise = s
                             in shuffle $ take s' allNames)

genFieldsWithoutEmpty :: [Identifier] -> Gen [Schema.Field]
genFieldsWithoutEmpty ts = genFields_ $ liftM (flip Schema.DataField) (elements ts)
