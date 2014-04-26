module Cauterize.Specification
  ( module Cauterize.Specification.Types
  , fromSchema
  ) where

import Cauterize.Specification.Types
import Cauterize.FormHash

import qualified Cauterize.Schema.Types as SC

fromSchema :: SC.Schema -> Specification
fromSchema (SC.Schema n v fs) =
  let spec = Specification n v (show $ formHash spec) (fromSchemaForms fs)
  in spec

fromSchemaForms :: [SC.SchemaForm] -> [SpecForm]
fromSchemaForms = map fromSchemaForm
  where
    fromSchemaForm (SC.FType f) = SpecForm
