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
  
  putStrLn hfile
  hres <- hastacheFile defaultConfig hfile context
  TL.putStrLn hres

  putStrLn cfile
  cres <- hastacheFile defaultConfig cfile context
  TL.putStrLn cres

data Info = Info {
  libname :: String
} deriving (Data, Typeable)

context :: MuContext IO
context = mkGenericContext $ Info { libname = "somename" }
