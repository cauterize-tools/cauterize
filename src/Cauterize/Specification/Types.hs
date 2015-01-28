{-# LANGUAGE FlexibleInstances, RecordWildCards, DeriveDataTypeable #-}
module Cauterize.Specification.Types
  ( Spec(..)
  , SpType(..)
  , Sized(..)

  , FixedSize(..)
  , RangeSize(..)

  , LengthRepr(..)
  , TagRepr(..)
  , FlagsRepr(..)

  , Depth(..)

  , fromSchema
  , prettyPrint
  , typeName
  , specTypeMap
  , typeDepthMap
  ) where

import Cauterize.FormHash
import Cauterize.Common.Types
import Data.List
import Data.Function
import Data.Maybe
import Data.Graph
import Data.Data

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import qualified Cauterize.Common.Types as CT
import qualified Cauterize.Schema.Types as SC

import Text.PrettyPrint
import Text.PrettyPrint.Class

data FixedSize = FixedSize { unFixedSize :: Integer }
  deriving (Show, Ord, Eq, Data, Typeable)
data RangeSize = RangeSize { rangeSizeMin :: Integer, rangeSizeMax :: Integer }
  deriving (Show, Ord, Eq, Data, Typeable)

data LengthRepr = LengthRepr { unLengthRepr :: BuiltIn }
  deriving (Show, Ord, Eq, Data, Typeable)
data TagRepr = TagRepr { unTagRepr :: BuiltIn }
  deriving (Show, Ord, Eq, Data, Typeable)
data FlagsRepr = FlagsRepr { unFlagsRepr :: BuiltIn }
  deriving (Show, Ord, Eq, Data, Typeable)

newtype Depth = Depth { unDepth :: Integer }
  deriving (Show, Ord, Eq, Data, Typeable)

mkRangeSize :: Integer -> Integer -> RangeSize
mkRangeSize mi ma = if mi > ma
                      then error $ "Bad range: " ++ show mi ++ " -> " ++ show ma ++ "."
                      else RangeSize mi ma

class Sized a where
  minSize :: a -> Integer
  maxSize :: a -> Integer

  minimumOfSizes :: [a] -> Integer
  minimumOfSizes [] = 0
  minimumOfSizes xs = minimum $ map minSize xs

  maximumOfSizes :: [a] -> Integer
  maximumOfSizes [] = 0
  maximumOfSizes xs = maximum $ map maxSize xs

  rangeFitting :: [a] -> RangeSize
  rangeFitting ss = mkRangeSize (minimumOfSizes ss) (maximumOfSizes ss)

  sumOfMinimums :: [a] -> Integer
  sumOfMinimums = sum . map minSize

  sumOfMaximums :: [a] -> Integer
  sumOfMaximums = sum . map maxSize

instance Sized FixedSize where
  minSize (FixedSize i) = i
  maxSize (FixedSize i) = i

instance Sized RangeSize where
  minSize (RangeSize i _) = i
  maxSize (RangeSize _ i) = i

instance Pretty FixedSize where
  pretty (FixedSize s) = parens $ text "fixed-size" <+> integer s

instance Pretty RangeSize where
  pretty (RangeSize mi ma) = parens $ text "range-size" <+> integer mi <+> integer ma

instance Pretty LengthRepr where
  pretty (LengthRepr bi) = parens $ text "length-repr" <+> pShow bi

instance Pretty TagRepr where
  pretty (TagRepr bi) = parens $ text "tag-repr" <+> pShow bi

instance Pretty FlagsRepr where
  pretty (FlagsRepr bi) = parens $ text "flags-repr" <+> pShow bi

instance Pretty Depth where
  pretty (Depth d) = parens $ text "depth" <+> integer d

data Spec = Spec { specName :: Name
                 , specVersion :: Version
                 , specHash :: FormHash
                 , specSize :: RangeSize
                 , specDepth :: Depth
                 , specTypes :: [SpType] }
  deriving (Show, Eq, Data, Typeable)

data SpType = BuiltIn      { unBuiltIn   :: TBuiltIn
                           , spHash      :: FormHash
                           , spFixedSize :: FixedSize }

            | Scalar       { unScalar     :: TScalar
                           , spHash       :: FormHash
                           , spFixedSize  :: FixedSize }

            | Const        { unConst     :: TConst
                           , spHash      :: FormHash
                           , spFixedSize :: FixedSize }

            | Array        { unFixed     :: TArray
                           , spHash      :: FormHash
                           , spRangeSize :: RangeSize }

            | Vector       { unBounded   :: TVector
                           , spHash      :: FormHash
                           , spRangeSize :: RangeSize
                           , lenRepr     :: LengthRepr }

            | Struct       { unStruct    :: TStruct
                           , spHash      :: FormHash
                           , spRangeSize :: RangeSize }

            | Set          { unSet       :: TSet
                           , spHash      :: FormHash
                           , spRangeSize :: RangeSize
                           , flagsRepr   :: FlagsRepr }

            | Enum         { unEnum      :: TEnum
                           , spHash      :: FormHash
                           , spRangeSize :: RangeSize
                           , tagRepr     :: TagRepr }

            | Pad          { unPad       :: TPad
                           , spHash      :: FormHash
                           , spFixedSize :: FixedSize }
  deriving (Show, Ord, Eq, Data, Typeable)

instance Sized SpType where
  minSize (BuiltIn { spFixedSize = s}) = minSize s
  minSize (Scalar { spFixedSize = s}) = minSize s
  minSize (Const { spFixedSize = s}) = minSize s
  minSize (Array { spRangeSize = s}) = minSize s
  minSize (Vector { spRangeSize = s}) = minSize s
  minSize (Struct { spRangeSize = s}) = minSize s
  minSize (Set { spRangeSize = s}) = minSize s
  minSize (Enum { spRangeSize = s}) = minSize s
  minSize (Pad { spFixedSize = s}) = minSize s

  maxSize (BuiltIn { spFixedSize = s}) = maxSize s
  maxSize (Scalar { spFixedSize = s}) = maxSize s
  maxSize (Const { spFixedSize = s}) = maxSize s
  maxSize (Array { spRangeSize = s}) = maxSize s
  maxSize (Vector { spRangeSize = s}) = maxSize s
  maxSize (Struct { spRangeSize = s}) = maxSize s
  maxSize (Set { spRangeSize = s}) = maxSize s
  maxSize (Enum { spRangeSize = s}) = maxSize s
  maxSize (Pad { spFixedSize = s}) = maxSize s

typeName :: SpType -> Name
typeName (BuiltIn { unBuiltIn = (TBuiltIn b)}) = show b
typeName (Scalar { unScalar = (TScalar n _)}) = n
typeName (Const { unConst = (TConst n _ _)}) = n
typeName (Array { unFixed = (TArray n _ _)}) = n
typeName (Vector { unBounded = (TVector n _ _)}) = n
typeName (Struct { unStruct = (TStruct n _)}) = n
typeName (Set { unSet = (TSet n _)}) = n
typeName (Enum { unEnum = (TEnum n _)}) = n
typeName (Pad { unPad = (TPad n _)}) = n

pruneBuiltIns :: [SpType] -> [SpType]
pruneBuiltIns fs = refBis ++ topLevel
  where
    (bis, topLevel) = L.partition isBuiltIn fs

    biNames = map (\(BuiltIn (TBuiltIn b) _ _) -> show b) bis
    biMap = M.fromList $ zip biNames bis

    rsSet = S.fromList $ concatMap referencesOf topLevel
    biSet = S.fromList biNames

    refBiNames = S.toList $ rsSet `S.intersection` biSet
    refBis = map snd $ M.toList $ M.filterWithKey (\k _ -> k `elem` refBiNames) biMap

    isBuiltIn (BuiltIn {..}) = True
    isBuiltIn _ = False

-- Topographically sort the types so that types with the fewest dependencies
-- show up first in the list of types. Types with the most dependencies are
-- ordered at the end. This allows languages that have order-dependencies to
-- rely on the sorted list for the order of code generation.
topoSort :: [SpType] -> [SpType]
topoSort sps = flattenSCCs . stronglyConnComp $ map m sps
  where
    m t = let n = typeName t
          in (t, n, referencesOf t)

-- TODO: Double-check the Schema hash can be recreated.
fromSchema :: SC.Schema -> Spec
fromSchema sc@(SC.Schema n v fs) = Spec n v overallHash (rangeFitting fs') (maximumTypeDepth sc) fs'
  where
    fs' = topoSort $ pruneBuiltIns $ map fromF fs
    keepNames = S.fromList $ map typeName fs'

    tyMap = SC.schemaTypeMap sc
    thm = typeHashMap sc
    hashScType name = fromJust $ name `M.lookup` thm

    overallHash = let a = hashInit `hashUpdate` n `hashUpdate` v
                      sorted = sortBy (compare `on` fst) $ M.toList thm
                      filtered = filter (\(x,_) -> x `S.member` keepNames) sorted
                      hashStrs = map (show . snd) filtered
                  in hashFinalize $ foldl hashUpdate a hashStrs

    specMap = fmap fromF tyMap
    fromF p = mkSpecType specMap p hash
      where
        hash = hashScType (SC.typeName p)

-- | This is responsible for building a map from names to hashes out of a
-- Schema.  Hashes are of a textual that uniquely represents the type. The
-- representation chosen for is as follows:
--
--   1. For builtins, hash the string representation of the builtin.
--   2. For other types without fields:
--     a. State the type prototype
--     b. State the type name
--     c. State any other field data
--   3. For other types with fields:
--     a. State hte type prototype
--     b. State the type name
--     c. State a textual representation of each field
--
-- Field data that represents other types should be replaced by the hash of
-- that type. Field data that's represented as a built-in should use the name
-- of the built-in.
--
-- Fields are represented by the word "field" followed by the field name, the
-- hash of the referenced type, and a textual representation of the field's
-- index.
--
-- Empty Fields are represented by the word "field" followed by the field name
-- and a textual representation of the field's index.
--
-- An example:
--
--   This type: (const foo u8 12)
--
--   ... is represented as the hash of the string ...
--
--   "const foo u8 +12"
--
-- Another example:
--
--   This type: (array bar 64 baz)
--
--   ... is represented as the hash of the string ...
--
--   "array bar [hash of baz] +64"
--
typeHashMap :: SC.Schema -> M.Map Name FormHash
typeHashMap s = m
  where
    m = fmap typeHash (SC.schemaTypeMap s)
    lu n = show (fromJust $ n `M.lookup` m)
    fieldStr (EmptyField n i) = ["field", n, showNumSigned i]
    fieldStr (Field n r i) = ["field", n, lu r, showNumSigned i]
    typeHash t =
      let str = case t of
                  SC.BuiltIn (TBuiltIn b) -> [show b]
                  SC.Scalar (TScalar n b) -> ["scalar", n, show b]
                  SC.Const (TConst n b i) -> ["const", n, show b, showNumSigned i]
                  SC.Array (TArray n r i) -> ["array", n, lu r, showNumSigned i]
                  SC.Vector (TVector n r i) -> ["vector", n, lu r, showNumSigned i]
                  SC.Struct (TStruct n (Fields fs)) -> ["struct", n] ++ concatMap fieldStr fs
                  SC.Set (TSet n (Fields fs)) -> ["set", n] ++ concatMap fieldStr fs
                  SC.Enum (TEnum n (Fields fs)) -> ["enum", n] ++ concatMap fieldStr fs
                  SC.Pad (TPad n i) -> ["pad", n, showNumSigned i]
      in hashString . unwords $ str

typeDepthMap :: SC.Schema -> M.Map Name Integer
typeDepthMap s = m
  where
    m = fmap typeDepth (SC.schemaTypeMap s)
    lu n = fromJust $ n `M.lookup` m

    fieldDepth (CT.Field { CT.fRef = r }) = lu r
    fieldDepth (CT.EmptyField {}) = 0

    maxFieldsDepth fs =
      let ds = map fieldDepth fs
      in maximum ds

    typeDepth :: SC.ScType -> Integer
    typeDepth t =
      case t of
        SC.BuiltIn (TBuiltIn {}) -> 1
        SC.Scalar (TScalar {}) -> 2
        SC.Const (TConst {}) -> 2
        SC.Array (TArray _ r _) -> 1 + lu r
        SC.Vector (TVector _ r _) -> 1 + lu r
        SC.Struct (TStruct _ (Fields fs)) -> 1 + maxFieldsDepth fs
        SC.Set (TSet _ (Fields fs)) -> 1 + maxFieldsDepth fs
        SC.Enum (TEnum _ (Fields fs)) -> 1 + maxFieldsDepth fs
        SC.Pad (TPad {}) -> 1

maximumTypeDepth :: SC.Schema -> Depth
maximumTypeDepth s = let m = typeDepthMap s
                     in Depth . maximum . M.elems $ m

showNumSigned :: (Ord a, Show a, Num a) => a -> String
showNumSigned v = let v' = abs v
                      v'' = show v'
                  in if v < 0
                       then '-':v''
                       else '+':v''

specTypeMap :: Spec -> M.Map Name SpType
specTypeMap s = let ts = specTypes s
                    ns = map typeName ts
                in M.fromList $ zip ns ts

mkSpecType :: M.Map Name SpType -> SC.ScType -> FormHash -> SpType
mkSpecType m p =
  case p of
    (SC.BuiltIn t@(TBuiltIn b)) ->
      let s = builtInSize b
      in \h -> BuiltIn t h (FixedSize s)
    (SC.Scalar  t@(TScalar _ b)) ->
      let s = builtInSize b
      in \h -> Scalar t h (FixedSize s)
    (SC.Const   t@(TConst _ b _)) ->
      let s = builtInSize b
      in \h -> Const t h (FixedSize s)
    (SC.Array t@(TArray _ r i)) ->
      let ref = lookupRef r
      in \h -> Array t h (mkRangeSize (i * minSize ref) (i * maxSize ref))
    (SC.Vector t@(TVector _ r i)) ->
      let ref = lookupRef r
          repr = minimalExpression i
          repr' = LengthRepr repr
          reprSz = builtInSize repr
      in \h -> Vector t h (mkRangeSize reprSz (reprSz + (i * maxSize ref))) repr'
    (SC.Struct t@(TStruct _ rs)) ->
      let refs = lookupRefs rs
          sumMin = sumOfMinimums refs
          sumMax = sumOfMaximums refs
      in \h -> Struct t h (mkRangeSize sumMin sumMax)
    (SC.Set t@(TSet _ rs)) ->
      let refs = lookupRefs rs
          sumMax = sumOfMaximums refs
          repr = minimalBitField (fieldsLength rs)
          repr' = FlagsRepr repr
          reprSz = builtInSize repr
      in \h -> Set t h (mkRangeSize reprSz (reprSz + sumMax)) repr'
    (SC.Enum t@(TEnum _ rs)) ->
      let refs = lookupRefs rs
          minMin = minimumOfSizes refs
          maxMax = maximumOfSizes refs
          repr = minimalExpression (fieldsLength rs)
          repr' = TagRepr repr
          reprSz = builtInSize repr
      in \h -> Enum t h (mkRangeSize (reprSz + minMin) (reprSz + maxMax)) repr'
    (SC.Pad t@(TPad _ l)) -> \h -> Pad t h (FixedSize l)
  where
    lookupRef r = fromJust $ r `M.lookup` m
    lookupField (Field _ r _) = Just $ lookupRef r
    lookupField (EmptyField _ _) = Nothing
    lookupRefs = mapMaybe lookupField . unFields

instance References SpType where
  referencesOf (BuiltIn {..}) = []
  referencesOf (Scalar s _ _) = referencesOf s
  referencesOf (Const  c _ _) = referencesOf c
  referencesOf (Array f _ _) = referencesOf f
  referencesOf (Vector b _ _ r) = nub $ show (unLengthRepr r) : referencesOf b
  referencesOf (Struct s _ _) = referencesOf s
  referencesOf (Set s _ _ r) = nub $ show (unFlagsRepr r) : referencesOf s
  referencesOf (Enum e _ _ r) = nub $ show (unTagRepr r) : referencesOf e
  referencesOf (Pad {..}) = []

prettyPrint :: Spec -> String
prettyPrint = show . pretty

pShow :: (Show a) => a -> Doc
pShow = text . show

pDQText :: String -> Doc
pDQText = doubleQuotes . text

instance Pretty Spec where
  pretty (Spec n v h sz d fs) = parens $ hang ps 1 pfs
    where
      ps = text "specification" <+> pDQText n <+> pDQText v <+> pretty h <+> pretty sz <+> pretty d
      pfs = vcat $ map pretty fs

-- When printing spec types, the following is the general order of fields
--  (type name hash [references] [representations] [lengths])
instance Pretty SpType where
  pretty (BuiltIn (TBuiltIn b) h sz) = parens $ pt <+> pa
    where
      pt = text "builtin" <+> pShow b <+> pretty h
      pa = pretty sz
  pretty (Scalar (TScalar n b) h sz) = parens $ pt $+$ nest 1 pa
    where
      pt = text "scalar" <+> text n <+> pretty h
      pa = pretty sz $$ pShow b
  pretty (Const (TConst n b i) h sz) = parens $ pt $+$ nest 1 pa
    where
      pt = text "const" <+> text n <+> pretty h
      pa = pretty sz $$ pShow b $$ integer i
  pretty (Array (TArray n m i) h sz) = parens $ pt $+$ nest 1 pa
    where
      pt = text "array" <+> text n <+> pretty h
      pa = pretty sz $$ integer i $$ text m
  pretty (Vector (TVector n m i) h sz bi) = parens $ pt $+$ nest 1 pa
    where
      pt = text "vector" <+> text n <+> pretty h
      pa = pretty sz $$ pretty bi $$ integer i $$ text m
  pretty (Struct (TStruct n rs) h sz) = prettyFieldedB0 "struct" n rs sz h
  pretty (Set (TSet n rs) h sz bi) = prettyFieldedB1 "set" n rs sz bi h
  pretty (Enum (TEnum n rs) h sz bi) = prettyFieldedB1 "enum" n rs sz bi h
  -- when printing/parsing padding, the length of the padding is always the min/max
  pretty (Pad (TPad n _) h sz) = parens pt
    where
      pt = text "pad" <+> text n <+> pretty h <+> pretty sz

-- Printing fielded-types involves hanging the name, the sizes, and the hash on
-- one line and the fields on following lines.
prettyFieldedB0 :: (Pretty sz) => String -> String -> Fields -> sz -> FormHash -> Doc
prettyFieldedB0 t n fs sz hash = parens $ hang pt 1 pfs
  where
    pt = text t <+> text n <+> pretty hash
    pfs = pretty sz $$ specPrettyFields fs

prettyFieldedB1 :: (Pretty sz, Pretty bi) => String -> String -> Fields -> sz -> bi -> FormHash -> Doc
prettyFieldedB1 t n fs sz repr hash = parens $ hang pt 1 pfs
  where
    pt = text t <+> text n <+> pretty hash
    pfs = pretty sz $$ pretty repr $$ specPrettyFields fs

specPrettyRefs :: Field -> Doc
specPrettyRefs  (EmptyField n i) = parens $ text "field" <+> text n <+> integer i
specPrettyRefs  (Field n m i) = parens $ text "field" <+> text n <+> text m <+> integer i

specPrettyFields :: Fields -> Doc
specPrettyFields (Fields fs) = parens $ hang (text "fields") 1 pfs
  where
    pfs = vcat $ map specPrettyRefs fs
