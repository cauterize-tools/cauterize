module Cauterize.Test.Generate.Options
  ( GenerateOpts(..)
  , genOptions
  , parseProtoParam
  , allProtoParams
  ) where

import Cauterize.Schema.Arbitrary
import qualified Data.Set as S
import Options.Applicative

data GenerateOpts = GenerateOpts
  { count :: Int
  } deriving (Show)

genOptions :: Parser GenerateOpts
genOptions = GenerateOpts
  <$> option auto
    ( long "count" <> metavar "COUNT" <> help countHelp )
  where
    countHelp = "The number of types to generate."

parseProtoParam :: String -> Either String ProtoParam
parseProtoParam "scalar" = Right ParamScalar
parseProtoParam "array" = Right ParamArray
parseProtoParam "vector" = Right ParamVector
parseProtoParam "struct" = Right ParamStruct
parseProtoParam "set" = Right ParamSet
parseProtoParam "enum" = Right ParamEnum
parseProtoParam s = Left s

allProtoParams :: S.Set ProtoParam
allProtoParams = S.fromList [ ParamScalar, ParamArray
                            , ParamVector, ParamStruct
                            , ParamSet , ParamEnum ]
