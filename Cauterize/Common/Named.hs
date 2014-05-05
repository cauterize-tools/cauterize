module Cauterize.Common.Named where

class CautName a where
  cautName :: a -> String
