module Cauterize.Schema 
  ( module Cauterize.Schema.Parser
  , module Cauterize.Schema.Types
  , module Cauterize.Schema.Utils
  , checkSchema
  ) where

import Cauterize.Schema.Parser
import Cauterize.Schema.Types
import Cauterize.Schema.Utils

import Cauterize.Common.Named
import Data.List

checkSchema :: Schema -> Maybe Schema
checkSchema s@(Schema _ _ fs) = if and [uniqueNames]
                                  then Just s
                                  else Nothing
  where
    uniqueNames = let names = map (\(FType t) -> cautName t) fs
                  in length names == (length . nub . sort) names
