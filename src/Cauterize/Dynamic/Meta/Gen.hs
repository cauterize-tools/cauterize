module Cauterize.Dynamic.Meta.Gen
  ( dynamicMetaGen
  , dynamicMetaGen'
  ) where

import Cauterize.Dynamic.Meta.Types
import Cauterize.Dynamic.Gen
import Control.Monad
import Test.QuickCheck.Gen
import qualified Cauterize.Specification as Spec
import qualified Data.Map as M

dynamicMetaGen :: Spec.Specification -> IO MetaType
dynamicMetaGen spec = generate $ dynamicMetaGen' spec

dynamicMetaGen' :: Spec.Specification -> Gen MetaType
dynamicMetaGen' spec = do
  n <- elements $ M.keys m
  liftM MetaType (dynamicGenType' spec n)
  where
    m = Spec.specTypeMap spec
