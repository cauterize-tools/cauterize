{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Cauterize.Common.Field where

import Cauterize.Common.Primitives

import qualified Data.Map as M
import Data.Maybe as M
import Data.Data

import Text.PrettyPrint

data Fields = Fields { unFields :: [Field] }
  deriving (Show, Ord, Eq, Data, Typeable)

fieldsLength :: Fields -> Int
fieldsLength (Fields fs) = length fs

data Field = Field { fName :: Name , fRef :: Name , fIndex :: Integer }
           | EmptyField { fName :: Name, fIndex :: Integer }
  deriving (Show, Ord, Eq, Data, Typeable)

refSig :: M.Map Name Signature -> Field -> Signature
refSig _ (EmptyField n _) = concat ["(field ", n, ")"]
refSig sm (Field n m _) = concat ["(field ", n, " ", luSig m, ")"]
  where
    luSig na = fromJust $ na `M.lookup` sm

refRef :: Field -> Maybe Name
refRef (EmptyField _ _) = Nothing
refRef (Field _ n _) = Just n

schemaPrettyRefs :: Field -> Doc
schemaPrettyRefs  (EmptyField n _) = parens $ text "field" <+> text n
schemaPrettyRefs  (Field n m _) = parens $ text "field" <+> text n <+> text m

specPrettyRefs :: Field -> Doc
specPrettyRefs  (EmptyField n i) = parens $ text "field" <+> text n <+> integer i
specPrettyRefs  (Field n m i) = parens $ text "field" <+> text n <+> text m <+> integer i

specPrettyFields :: Fields -> Doc
specPrettyFields (Fields fs) = parens $ hang (text "fields") 1 pfs
  where
    pfs = vcat $ map specPrettyRefs fs

schemaPrettyFields :: Fields -> Doc
schemaPrettyFields (Fields fs) = parens $ hang (text "fields") 1 pfs
  where
    pfs = vcat $ map schemaPrettyRefs fs
