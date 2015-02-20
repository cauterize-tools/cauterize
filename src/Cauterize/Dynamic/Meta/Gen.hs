module Cauterize.Dynamic.Meta.Gen
  ( dynamicMetaGen
  ) where

import Cauterize.Dynamic.Meta.Types
import Cauterize.Dynamic.Gen
import Control.Monad
import Test.QuickCheck.Gen
import qualified Cauterize.Meta as Meta
import qualified Cauterize.Specification as Spec
import qualified Data.Map as M

dynamicMetaGen :: Spec.Spec -> Meta.Meta -> IO MetaType
dynamicMetaGen spec meta = generate $ dynamicMetaGen' spec meta

dynamicMetaGen' :: Spec.Spec -> Meta.Meta -> Gen MetaType
dynamicMetaGen' spec meta = do
  n <- elements $ M.keys m
  liftM MetaType (dynamicGenType' spec n)
  where
    m = Meta.metaTypeMap meta
