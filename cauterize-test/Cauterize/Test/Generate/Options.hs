module Cauterize.Test.Generate.Options
  ( GenerateOpts(..)
  , genOptions
  , parseProtoParam
  , allProtoParams
  ) where

import Cauterize.Generate
import Options.Applicative

data GenerateOpts = GenerateOpts
  { count :: Integer
  , size :: Integer
  } deriving (Show)

genOptions :: Parser GenerateOpts
genOptions = GenerateOpts
  <$> option auto
    ( long "count" <> metavar "COUNT" <> help countHelp )
  <*> option auto
    ( long "size" <> metavar "SIZE" <> help sizeHelp )
  where
    countHelp = "The number of types to generate."
    sizeHelp = "The maximum number of bytes to allow in the encoded representation."

parseProtoParam :: String -> Either String PrototypeVariant
parseProtoParam "synonym" = Right PVSynonym
parseProtoParam "array" = Right PVArray
parseProtoParam "vector" = Right PVVector
parseProtoParam "record" = Right PVRecord
parseProtoParam "combination" = Right PVCombination
parseProtoParam "union" = Right PVUnion
parseProtoParam s = Left s

allProtoParams :: [PrototypeVariant]
allProtoParams = [minBound .. maxBound]
