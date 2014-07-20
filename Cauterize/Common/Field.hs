{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Cauterize.Common.Field where

import Cauterize.Common.Primitives

import qualified Data.Map as M
import Data.Maybe as M
import Data.Data

import Text.PrettyPrint

data Fields r = Fields { unFields :: [Field r] }
  deriving (Show, Ord, Eq, Data, Typeable)

fieldsLength :: Fields r -> Int
fieldsLength (Fields fs) = length fs

data Field r = Field { fieldName :: Name
                     , fieldRef :: r
                     , fieldIndex :: Integer
                     }
  deriving (Show, Ord, Eq, Data, Typeable)

refSig :: M.Map Name Signature -> Field Name -> Signature
refSig sm (Field n m _) = concat ["(field ", n, " ", luSig m, ")"]
  where
    luSig na = fromJust $ na `M.lookup` sm

refRef :: Field t -> t
refRef (Field _ n _) = n

schemaPrettyRefs :: Field Name -> Doc
schemaPrettyRefs  (Field n "void" _) = parens $ text "field" <+> text n
schemaPrettyRefs  (Field n m _) = parens $ text "field" <+> text n <+> text m

specPrettyRefs :: Field Name -> Doc
specPrettyRefs  (Field n m i) = parens $ text "field" <+> text n <+> text m <+> integer i

specPrettyFields :: Fields String -> Doc
specPrettyFields (Fields fs) = parens $ hang (text "fields") 1 pfs
  where
    pfs = vcat $ map specPrettyRefs fs

schemaPrettyFields :: Fields String -> Doc
schemaPrettyFields (Fields fs) = parens $ hang (text "fields") 1 pfs
  where
    pfs = vcat $ map schemaPrettyRefs fs
