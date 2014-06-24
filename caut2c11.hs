{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified Data.Text.Lazy.IO as TL
import Text.Hastache
import Text.Hastache.Context
import Data.Data 
import Control.Monad
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Options.Applicative

import Paths_cauterize

import Cauterize.Schema as SC
import Cauterize.Specification as SP
import Cauterize.Schema.Arbitrary

data Caut2C11Opts = Caut2C11Opts
  { inputFile :: String
  , outputDirectory :: String
  } deriving (Show)

optParser :: Parser Caut2C11Opts
optParser = Caut2C11Opts
  <$> strOption
    ( long "input"
   <> metavar "FILE_PATH"
   <> help "Input Cauterize schema file."
    )
  <*> strOption
    ( long "output"
   <> metavar "DIRECTORY_PATH"
   <> help "Output Cauterize directory."
    )

options :: ParserInfo Caut2C11Opts
options = info (optParser <**> helper)
            ( fullDesc
           <> progDesc "Process Cauterize schema files."
            )

runWithOptions :: (Caut2C11Opts -> IO ()) -> IO ()
runWithOptions fn = execParser options >>= fn

main :: IO ()
main = runWithOptions caut2c11

caut2c11 :: Caut2C11Opts -> IO ()
caut2c11 opts = do
  hfile <- getDataFileName "libroot.tmpl.h"
  cfile <- getDataFileName "libroot.tmpl.c"

  s <- liftM unValidSchema $ generate (arbitrary :: Gen ValidSchema)

  let context = mkGenericContext templInfo
  
  putStrLn hfile
  hres <- hastacheFile muConfig hfile context
  TL.putStrLn hres

  putStrLn cfile
  cres <- hastacheFile muConfig cfile context
  TL.putStrLn cres

templInfo :: Info
templInfo = Info
  { libname = "special"
  , types = Types
    { enumTypes =
      [ EnumInfo
        { enumName = "anEnum"
        , enumFields =
          [ Field { fieldName = "foo", fieldRef = "u8", fieldIndex = 0 }
          , Field { fieldName = "bar", fieldRef = "u8", fieldIndex = 1 }
          ]
        }
      , EnumInfo
        { enumName = "bnEnum"
        , enumFields =
          [ Field { fieldName = "bing", fieldRef = "u8", fieldIndex = 0 }
          , Field { fieldName = "boop", fieldRef = "u8", fieldIndex = 1 }
          ]
        }
      ] } }

data Info = Info
  { libname :: String
  , types :: Types
  } deriving (Data, Typeable)

data Types = Types
  { enumTypes :: [EnumInfo]
  } deriving (Data, Typeable)

data EnumInfo = EnumInfo
  { enumName :: String
  , enumFields :: [Field]
  } deriving (Data, Typeable)

data Field = Field
  { fieldName :: String
  , fieldRef :: String
  , fieldIndex :: Integer
  } deriving (Data, Typeable)

-- muConfig :: MonadIO m => MuConfig m
muConfig = defaultConfig { muEscapeFunc = emptyEscape }
