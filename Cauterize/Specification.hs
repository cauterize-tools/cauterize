module Cauterize.Specification
  ( annotateHash
  ) where

import Cauterize.FormHash
import Data.Maybe

import qualified Data.Map as M
import qualified Cauterize.Schema.Types as SC

annotateHash :: (Ord a) => SC.Schema SC.Name a -> SC.Schema SC.Name FormHash
annotateHash s@(SC.Schema n v fs) = SC.Schema n v (map ann fs)
  where
    sigMap = SC.schemaSigMap s
    getSig t = fromJust $ t `M.lookup` sigMap

    ann :: SC.SchemaForm SC.Name a -> SC.SchemaForm SC.Name FormHash
    ann (SC.FType t) = SC.FType $ SC.annotateWith t (hashString . getSig . SC.typeName)
