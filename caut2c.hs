{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Paths_cauterize

import qualified Data.Text.Lazy.IO as TL
import Text.Hastache
import Text.Hastache.Context

import Data.Data 

main :: IO ()
main = do
  hfile <- getDataFileName "libroot.tmpl.h"
  cfile <- getDataFileName "libroot.tmpl.c"
  enumTmpl <- getDataFileName "types/enum.tmpl.h"

  let context = mkGenericContext $ info
  
  putStrLn hfile
  hres <- hastacheFile muConfig hfile context
  TL.putStrLn hres

  putStrLn cfile
  cres <- hastacheFile muConfig cfile context
  TL.putStrLn cres

info :: Info
info = Info
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
