module Cauterize.Common.Named where

class CautName a where
  cautName :: a -> String

class RefersNames a where
  referredNames :: a -> [String]
