module Cauterize.Dynamic.Meta.Gen
  ( dynamicMetaGen
  ) where

import Cauterize.Dynamic.Meta.Types
import Cauterize.Dynamic.Gen
import Control.Monad
import Test.QuickCheck.Gen
import qualified Cauterize.Specification as Spec
import qualified Data.Map as M

dynamicMetaGen :: Spec.Spec -> IO MetaType
dynamicMetaGen spec = generate $ dynamicMetaGen' spec

dynamicMetaGen' :: Spec.Spec -> Gen MetaType
dynamicMetaGen' spec = do
  n <- elements $ M.keys m
  liftM MetaType (dynamicGenType' spec n)
  where
    m = Spec.specTypeMap spec
