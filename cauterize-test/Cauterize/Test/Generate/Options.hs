module Cauterize.Test.Generate.Options
  ( GenerateOpts(..)
  , genOptions
  ) where

import Data.List.Split
import Cauterize.Generate
import Options.Applicative

data GenerateOpts = GenerateOpts
  { count :: Integer
  , size :: Integer
  , prototypes :: [PrototypeVariant]
  } deriving (Show)

genOptions :: Parser GenerateOpts
genOptions = GenerateOpts
  <$> option auto
    ( long "count" <> metavar "COUNT" <> help countHelp )
  <*> option auto
    ( long "size" <> metavar "SIZE" <> help sizeHelp )
  <*> option (str >>= parseOptPrototypes)
       ( long "prototypes"
      <> metavar "PROTOTYPES"
      <> value defaultAllowedPrototypes
      <> help prototypesHelp
       )
  where
    countHelp = "The number of types to generate."
    sizeHelp = "The maximum number of bytes to allow in the encoded representation."
    prototypesHelp = concat [ "Which prototypes to include in schema generation. "
                            , "Define using a comma-separated list including only "
                            , "the following elements: array,combination,record,synonym,union,vector."
                            ]

parseOptPrototypes :: Monad m => String -> m [PrototypeVariant]
parseOptPrototypes s = do
  let protStrs = splitOn "," s
  mapM parseOptPrototype protStrs

parseOptPrototype :: Monad m => String -> m PrototypeVariant
parseOptPrototype "synonym" = return PVSynonym
parseOptPrototype "array" = return PVArray
parseOptPrototype "vector" = return PVVector
parseOptPrototype "record" = return PVRecord
parseOptPrototype "combination" = return PVCombination
parseOptPrototype "union" = return PVUnion
parseOptPrototype s = fail s
