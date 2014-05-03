module Cauterize.Schema 
  ( module Cauterize.Schema.Parser
  , module Cauterize.Schema.Types
  , module Cauterize.Schema.Utils
  , checkSchema
  ) where

import Cauterize.Schema.Parser
import Cauterize.Schema.Types
import Cauterize.Schema.Utils

import Data.Maybe
import qualified Data.Map as M

import Cauterize.Common.Named

data SchemaErrors = DuplicateNames [Name]
                  | Cycles [Cycle]
  deriving (Show)

checkSchema :: Schema -> [SchemaErrors]
checkSchema s@(Schema _ _ fs) = catMaybes [duplicateNames, cycles]
  where
    duplicateNames = case duplicates $ map (\(FType t) -> cautName t) fs of
                        [] -> Nothing
                        ds -> Just $ DuplicateNames ds
    cycles = case schemaCycles s of
                [] -> Nothing
                cs -> Just $ Cycles cs


duplicates :: (Eq a, Ord a) => [a] -> [a]
duplicates ins = map fst $ M.toList dups
  where
    dups = M.filter (>1) counts
    counts = foldl insertWith M.empty ins
    insertWith m x = M.insertWith ((+) :: (Int -> Int -> Int)) x 1 m
