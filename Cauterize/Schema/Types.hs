module Cauterize.Schema.Types where

import Cauterize.Common.BuiltIn
import Cauterize.Common.Named
import Cauterize.FormHash
import Data.List
import Data.Maybe

import Data.Graph

import Text.PrettyPrint
import Text.PrettyPrint.Class

import qualified Data.Map as M

type Name = String
type Signature = String
type Cycle = [Name]
type TypeIdMap = M.Map Name FormHash

data Schema t a = Schema
  { schemaName :: String
  , schemaVersion :: String
  , schemaForms :: [SchemaForm t a]
  }
  deriving (Show)

data SchemaForm t a = FType (Type t a)
  deriving (Show)

data Type t a = TBuiltIn BuiltIn a
              | TScalar String BuiltIn a
              | TConst String BuiltIn Integer a

              | TFixedArray String t Integer a
              | TBoundedArray String t Integer a

              | TStruct String [IndexedRef t] a
              | TSet String [IndexedRef t] a

              | TEnum String [IndexedRef t] a
              | TPartial String [IndexedRef t] a

              | TPad String Integer a
  deriving (Show, Ord, Eq)

data IndexedRef t = IndexedRef String t Integer
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

typeSig :: (Ord a) => M.Map Name Signature -> Type String a -> Signature 
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

refSig :: M.Map Name Signature -> IndexedRef String -> Signature
refSig sm (IndexedRef n m _) = concat ["(field ", n, " ", luSig m, ")"]
  where
    luSig na = fromJust $ na `M.lookup` sm

schemaSigMap :: (Ord a) => Schema String a -> M.Map Name Signature
schemaSigMap schema = resultMap
  where
    tyMap = schemaTypeMap schema
    resultMap = fmap (typeSig resultMap) tyMap
{-


schemaTypeIdMap schema = case schemaCycles schema of
                          [] -> Right resultMap
                          cs -> Left cs
  where
    tyMap = schemaTypeMap schema
    resultMap = fmap hashType tyMap

    -- YO! There's a fromJust here. The way the input map is constructed
    -- should keep us from having to worry about this.
    hashType t = let dirRefs = fromJust $ mapM (`M.lookup` resultMap) (referredNames t)
                 in finalize $ foldl formHashWith (formHashCtx t) dirRefs

-}
{-
  
-- | This function serves two purposes:
--    1. If there are cycles in the schema, they are reported.
--    2. If the schema is valid, then a Map of names to Type IDs are produced.
schemaTypeIdMap :: Schema String a -> Either [Cycle] TypeIdMap
schemaTypeIdMap schema = case schemaCycles schema of
                          [] -> Right resultMap
                          cs -> Left cs
  where
    tyMap = schemaTypeMap schema
    resultMap = fmap hashType tyMap

    -- YO! There's a fromJust here. The way the input map is constructed
    -- should keep us from having to worry about this.
    hashType t = let dirRefs = fromJust $ mapM (`M.lookup` resultMap) (referredNames t)
                 in finalize $ foldl formHashWith (formHashCtx t) dirRefs

schemaCycles :: Schema String a -> [Cycle]
schemaCycles s = typeCycles (map snd $ M.toList tyMap)
  where
    tyMap = schemaTypeMap s


typeCycles :: [Type String a] -> [Cycle]
typeCycles ts = let ns = map (\t -> (cautName t, cautName t, referredNames t)) ts
                in mapMaybe isScc (stronglyConnComp ns)
  where
    isScc (CyclicSCC vs) = Just vs
    isScc _ = Nothing

schemaStructuralHash :: Schema String a -> FormHash
schemaStructuralHash s@(Schema n v fs) =
    let (Right m) = schemaTypeIdMap s
        ctx = hashInit `hashFn` n `hashFn` v
    in finalize $ foldl (hshFn m) ctx fs
  where
    hshFn :: TypeIdMap -> HashContext -> SchemaForm t a -> HashContext
    hshFn m ctx (FType t) = let tyId = fromJust $ cautName t `M.lookup` m
                            in ctx `formHashWith` tyId

referredNames :: Type Name a -> [Name]
referredNames (TBuiltIn _ _) = []
referredNames (TScalar _ b _) = [cautName b]
referredNames (TConst _ b _ _) = [cautName b]
referredNames (TFixedArray _ n _ _) = [n]
referredNames (TBoundedArray _ n _ _) = [n]
referredNames (TStruct _ fs _) = nub $  map (\(IndexedRef _ n _) -> n) fs
referredNames (TSet _ fs _) = nub $  map (\(IndexedRef _ n _) -> n) fs
referredNames (TEnum _ vs _) = nub $ map (\(IndexedRef _ n _) -> n) vs
referredNames (TPartial _ fs _) = nub $ map (\(IndexedRef _ n _) -> n) fs
referredNames (TPad _ _ _) = []

instance CautName (Type t a) where
  cautName (TBuiltIn b _) = show b
  cautName (TScalar n _ _) = n
  cautName (TConst n _ _ _) = n
  cautName (TFixedArray n _ _ _) = n
  cautName (TBoundedArray n _ _ _) = n
  cautName (TStruct n _ _) = n
  cautName (TSet n _ _) = n
  cautName (TEnum n _ _) = n
  cautName (TPartial n _ _) = n
  cautName (TPad n _ _) = n

-- Note: these instances only hash on the *value* of the type. This hash does
-- not take into account the structure of depended-uppon types.
instance (Hashable t) => Hashable (Type t a) where
  formHashWith ctx (TBuiltIn b _) = ctx `hashFn` "built-in" `formHashWith` b
  formHashWith ctx (TScalar n b _) = ctx `hashFn` "scalar" `hashFn` n `formHashWith` b
  formHashWith ctx (TConst n b i _) = ctx `hashFn` "const" `hashFn` n `formHashWith` b `hashFn` padShowInteger i
  formHashWith ctx (TFixedArray n m i _) = ctx `hashFn` "fixed" `hashFn` n `formHashWith` m `hashFn` padShowInteger i
  formHashWith ctx (TBoundedArray n m i _) = ctx `hashFn` "bounded" `hashFn` n `formHashWith` m `hashFn` padShowInteger i
  formHashWith ctx (TStruct n sfs _) = ctx `hashFn` "struct" `hashFn` n `formHashWith` sfs
  formHashWith ctx (TSet n sfs _) = ctx `hashFn` "set" `hashFn` n `formHashWith` sfs
  formHashWith ctx (TEnum n evs _) = ctx `hashFn` "enum" `hashFn` n `formHashWith` evs
  formHashWith ctx (TPartial n pfs _) = ctx `hashFn` "partial" `hashFn` n `formHashWith` pfs
  formHashWith ctx (TPad n i _) = ctx `hashFn` "pad" `hashFn` n `hashFn` padShowInteger i

instance (Hashable t) => Hashable (IndexedRef t) where
  formHashWith ctx (IndexedRef n m i) = ctx `hashFn` n `formHashWith` m `hashFn` padShowInteger i
  
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
  
padShowInteger :: Integer -> String
padShowInteger v = let w = 20
                       v' = abs v
                       v'' = show v'
                       num = replicate (w - length v'') '0' ++ v''
                   in if v < 0
                        then '-':num
                        else '+':num
