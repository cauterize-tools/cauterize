module Cauterize.Specification
  ( module Cauterize.Specification.Types
  , fromSchema
  , references
  , schemaTypeMap
  ) where

import Cauterize.Specification.Types
import Cauterize.FormHash
import Cauterize.Common.BuiltIn
import Cauterize.Common.Named
import qualified Cauterize.Schema.Types as SC

import Data.Bits
import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString as B

type Name = String
type Error = String

fromSchema :: SC.Schema -> Maybe Specification
fromSchema (SC.Schema n v fs) =
  let spec = Specification n v (show $ formHash spec) (fromSchemaForms fs)
  in Just spec

fromSchemaForms :: [SC.SchemaForm] -> [SpecForm]
fromSchemaForms = map fromSchemaForm
  where
    fromSchemaForm (SC.FType f) = SpecForm $ fromSchemaType f

schemaTypeIdRefMap :: M.Map Name SC.Type -> M.Map Name (Maybe (FormHash, [Name]))
schemaTypeIdRefMap m = r
  where
    r = fmap hashType m
    hashType :: SC.Type -> (Maybe FormHash, [Name])
    hashType t = (undefined, refs)
      where
        refs = concat $ maybeToList $ m `references` (cautName t)
        dirRefs = mapM (liftM fst . (`M.lookup` r)) (referredNames t) >>= sequence >>= magicHash -- :: Maybe [FormHash]


-- schemaTypeIdRefMap :: M.Map Name SC.Type -> M.Map Name (Maybe (FormHash, [Name]))
-- schemaTypeIdRefMap m = fmap hashType m
--   where
--     hashType :: SC.Type -> Maybe (FormHash, [Name])
--     hashType t = do
--       refs <- references m (cautName t)
--       return (undefined, refs)

schemaTypeMap :: SC.Schema -> M.Map Name SC.Type
schemaTypeMap (SC.Schema _ _ fs) = M.fromList $ map (\(SC.FType t) -> (cautName t, t)) fs

references :: M.Map Name SC.Type -> Name -> Maybe [Name]
references m n = do
  t <- n `M.lookup` m

  let rns = referredNames t
  let refs = map (maybeToList . references m) rns

  return $ n : (concat . concat) refs

-- schemaTypeInfo :: M.Map Name SC.Type -> M.Map Name (Either Error FormHash, [Name])
-- schemaTypeInfo = fmap hashRefs
--   where
--     hashRefs :: SC.Type -> (Either Error FormHash, [Name])
--     hashRefs t = let n = cautName n
--                  in 

fromSchemaType :: SC.Type -> Type
fromSchemaType (SC.TBuiltIn b) = TBuiltIn b
fromSchemaType (SC.TScalar n b) = TScalar n b
fromSchemaType (SC.TConst n b i) = TConst n b i
fromSchemaType (SC.TFixedArray n m i) = TFixedArray n m i
fromSchemaType (SC.TBoundedArray n m i) = TBoundedArray n m i (minimalExpression i)
fromSchemaType (SC.TStruct n fs) = TStruct n (fromSchemaStructFields fs)
fromSchemaType (SC.TSet n fs) = TSet n (minimalBitField $ length fs) (fromSchemaSetFields fs)
fromSchemaType (SC.TEnum n vs) = TEnum n (minimalExpression $ length vs) (fromSchemaEnumVariants vs)
fromSchemaType (SC.TPartial n l vs) = TPartial n l (minimalExpression l) (fromSchemaPartialVariants vs)
fromSchemaType (SC.TPad n l) = TPad n l

fromSchemaStructFields :: [SC.StructField] -> [StructField]
fromSchemaStructFields = map go
  where
    go (SC.StructField n t) = StructField n t

fromSchemaSetFields :: [SC.SetField] -> [SetField]
fromSchemaSetFields fs = snd $ mapAccumL go 0 fs
  where
    go :: Int -> SC.SetField -> (Int, SetField)
    go a (SC.SetField n m) = (a + 1, SetField n m (1 `shiftL` a))

fromSchemaEnumVariants :: [SC.EnumVariant] -> [EnumVariant]
fromSchemaEnumVariants vs = snd $ mapAccumL go 0 vs
  where
    go :: Int -> SC.EnumVariant -> (Int, EnumVariant)
    go a (SC.EnumVariant n m) = (a + 1, EnumVariant n m (fromIntegral a))

fromSchemaPartialVariants :: [SC.PartialVariant] -> [PartialVariant]
fromSchemaPartialVariants = map go
  where
    go :: SC.PartialVariant -> PartialVariant
    go (SC.PartialVariant n t) = PartialVariant n t (FormHash $ B.pack [0])

-- import Data.Maybe
-- uniquePrefixes :: Eq a => [[a]] -> Maybe [[a]]
-- uniquePrefixes ls = listToMaybe . dropper . map nub . transpose . map inits $ ls
--   where
--     dropper = dropWhile (\l -> length l < length ls) 
