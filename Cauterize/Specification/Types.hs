module Cauterize.Specification.Types where

import Cauterize.FormHash

data Specification = Specification
  { name :: String
  , version :: String
  , hash :: String
  , forms :: [SpecForm]
  }
  deriving (Show)

data SpecForm = SpecForm
  deriving (Show)

instance Hashable Specification where
  formHashWith ctx (Specification n v _ fs) = finalCtx
    where
      nextCtx = foldl hashFn ctx ["specification", n, v]
      finalCtx = foldl formHashWith nextCtx fs

instance Hashable SpecForm where
  formHashWith ctx _ = ctx `hashFn` "specform"
