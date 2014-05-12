module Cauterize.Specification
  ( -- annotateHash
  ) where

import Cauterize.FormHash
import Cauterize.Common.Primitives
import Data.Maybe

import qualified Data.Map as M
import qualified Cauterize.Schema.Types as SC

{-
annotateHash :: (Ord a) => SC.Schema Name a -> SC.Schema Name FormHash
annotateHash s@(SC.Schema n v fs) = SC.Schema n v (map ann fs)
  where
    sigMap = SC.schemaSigMap s
    getSig t = fromJust $ t `M.lookup` sigMap

    ann :: SC.SchemaForm Name a -> SC.SchemaForm Name FormHash
    ann (SC.FType t) = SC.FType $ SC.annotateWith t (hashString . getSig . SC.typeName)
    -}
