module Cauterize.Dynamic.Meta.Pretty
  ( dynamicMetaPretty
  ) where

import Cauterize.Specification
import Cauterize.Dynamic.Pretty
import Cauterize.Dynamic.Meta.Types

import qualified Data.Text.Lazy as T

dynamicMetaPretty :: Spec -> MetaType -> T.Text
dynamicMetaPretty s (MetaType t) = dynamicPretty s t
