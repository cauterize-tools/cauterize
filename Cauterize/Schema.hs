module Cauterize.Schema 
  ( module Cauterize.Schema.Parser
  , module Cauterize.Schema.Types
  , module Cauterize.Schema.Utils
  , checkSchema
  , schemaTypeIdMap
  ) where

import Cauterize.Schema.Parser
import Cauterize.Schema.Types
import Cauterize.Schema.Utils
import Cauterize.FormHash

import Data.List
import Data.Maybe
import qualified Data.Map as M

import Cauterize.Common.Named

import Data.Graph

type Name = String
type Cycle = [Name]

checkSchema :: Schema -> Maybe Schema
checkSchema s@(Schema _ _ fs) = if and [uniqueNames]
                                  then Just s
                                  else Nothing
  where
    uniqueNames = let names = map (\(FType t) -> cautName t) fs
                  in length names == (length . nub . sort) names

-- | This function serves two purposes:
--    1. If there are cycles in the schema, they are reported.
--    2. If the schema is valid, then a Map of names to Type IDs are produced.
schemaTypeIdMap :: Schema -> Either [Cycle] (M.Map Name FormHash)
schemaTypeIdMap schema = case typeCycles (map snd $ M.toList tyMap) of
                          [] -> Right resultMap
                          cs -> Left cs
  where
    schemaTypeMap (Schema _ _ fs) = M.fromList $ map (\(FType t) -> (cautName t, t)) fs
    tyMap = schemaTypeMap schema
    resultMap = fmap hashType tyMap

    -- YO! There's a fromJust here. The way the input map is constructed
    -- should keep us from having to worry about this.
    hashType t = let dirRefs = fromJust $ mapM (`M.lookup` resultMap) (referredNames t)
                 in finalize $ foldl formHashWith (formHashCtx t) dirRefs

typeCycles :: [Type] -> [Cycle]
typeCycles ts = let ns = map (\t -> (cautName t, cautName t, referredNames t)) ts
                in mapMaybe isScc (stronglyConnComp ns)
  where
    isScc (CyclicSCC vs) = Just vs
    isScc _ = Nothing

