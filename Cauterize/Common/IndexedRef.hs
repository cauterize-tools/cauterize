{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Common.IndexedRef where

import Cauterize.Common.Primitives

import qualified Data.Map as M
import Data.Maybe as M

import Text.PrettyPrint

data Fields r = Fields { unFields :: [IndexedRef r] }
  deriving (Show, Ord, Eq)

fieldsLength :: Fields r -> Int
fieldsLength (Fields fs) = length fs

data IndexedRef r = IndexedRef Name r Integer
  deriving (Show, Ord, Eq)

refSig :: M.Map Name Signature -> IndexedRef Name -> Signature
refSig sm (IndexedRef n m _) = concat ["(field ", n, " ", luSig m, ")"]
  where
    luSig na = fromJust $ na `M.lookup` sm

refRef :: IndexedRef t -> t
refRef (IndexedRef _ n _) = n

schemaPrettyRefs :: IndexedRef Name -> Doc
schemaPrettyRefs  (IndexedRef n "void" _) = parens $ text "field" <+> text n
schemaPrettyRefs  (IndexedRef n m _) = parens $ text "field" <+> text n <+> text m

specPrettyRefs :: IndexedRef Name -> Doc
specPrettyRefs  (IndexedRef n m i) = parens $ text "field" <+> text n <+> text m <+> integer i

specPrettyFields :: Fields String -> Doc
specPrettyFields (Fields fs) = parens $ hang (text "fields") 1 pfs
  where
    pfs = vcat $ map specPrettyRefs fs

schemaPrettyFields :: Fields String -> Doc
schemaPrettyFields (Fields fs) = parens $ hang (text "fields") 1 pfs
  where
    pfs = vcat $ map schemaPrettyRefs fs
