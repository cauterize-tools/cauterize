module Cauterize.Generate
  ( generateSchema
  , generateSchemaWith

  , PrototypeVariant(..)

  , defaultMaximumTypes
  , defaultMaximumSize
  , defaultAllowedPrototypes
  , defaultMargin
  ) where

import Cauterize.Common.Types
import Control.Monad
import Test.QuickCheck.Gen
import qualified Cauterize.Schema as Schema
import qualified Cauterize.Specification as Spec

data PrototypeVariant
  = PVSynonym
  | PVArray
  | PVVector
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
                       (go maximumTypes maximumSize allNames schemaBuiltIns)
  where
    -- Returns the size of the schema
    size :: [Schema.ScType] -> Integer
    size ts = let s = Spec.specSize . Spec.fromSchema $ Schema.Schema "temp_schema" "0" ts
              in Spec.maxSize s

    go :: Integer -> Integer -> [String] -> [Schema.ScType] -> Gen [Schema.ScType]
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

genVariant :: [Schema.ScType] -> [PrototypeVariant] -> String -> Gen Schema.ScType
genVariant [] _ _ = error "Cannot create new prototype without preexisting types."
genVariant _ [] _ = error "Must specify at least one prototype variant."
genVariant existingTypes variants name = do
  v <- elements variants
  case v of
    PVSynonym -> genSynonym
    PVArray -> genArray
    PVVector -> genVector
    PVRecord -> genRecord
    PVCombination -> genCombination
    PVUnion -> genUnion
  where
    existingNames = map Schema.typeName existingTypes
    genSynonym = liftM (Schema.Synonym . TSynonym name) (elements bis)
    genArray = liftM2 (\t l -> Schema.Array $ TArray name t l) (elements existingNames) arbLength
    genVector = liftM2 (\t l -> Schema.Vector $ TVector name t l) (elements existingNames) arbLength
    genRecord = liftM (Schema.Record . TRecord name) (genFieldsWithoutEmpty existingNames)
    genCombination = liftM (Schema.Combination . TCombination name) (genFields existingNames)
    genUnion = liftM (Schema.Union . TUnion name) (genFields existingNames)

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
schemaNames :: [String]
schemaNames = take 100 allNames

allNames :: [String]
allNames = let syms = ["a","e","i","o","u","y"]
           in map concat $ sequences syms

sequences :: [a] -> [[a]]
sequences ls = ls' ++ [i ++ [a] | i <- sequences ls, a <- ls]
  where
    ls' = map (:[]) ls

-- Functions for working with BuiltIn types.
bis :: [BuiltIn]
bis = [minBound .. maxBound]

biTypes :: [TBuiltIn]
biTypes = map TBuiltIn bis

schemaBuiltIns :: [Schema.ScType]
schemaBuiltIns = map Schema.BuiltIn biTypes

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
genFields_ :: (Num b, Enum b) => Gen (String -> b -> c) -> Gen [c]
genFields_ cstorGen = do
  count <- fieldCount
  fieldCstors <- replicateM count cstorGen
  let withNames = zipWith ($) fieldCstors allNames
  let withIndicies = zipWith ($) withNames [0..]
  return withIndicies
  where
    fieldCount = let ixs = [1..64]
                     freqs = reverse ixs
                     gens = map return ixs
                 in frequency $ zip freqs gens

genFields :: [String] -> Gen Fields
genFields ts = liftM Fields gf
  where gf = genFields_ $ frequency [(1, liftM (\d n ix -> Field n d ix) (elements ts))
                                    ,(1, return EmptyField)
                                    ]

genFieldsWithoutEmpty :: [String] -> Gen Fields
genFieldsWithoutEmpty ts = liftM Fields gf
  where gf = genFields_ $ liftM (\d n ix -> Field n d ix) (elements ts)
