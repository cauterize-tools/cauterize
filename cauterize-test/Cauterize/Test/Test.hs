module Cauterize.Test.Test
  ( runTest
  ) where

import qualified Cauterize.Test.Test.Options as O
import qualified Cauterize.Specification as S
import qualified Cauterize.Meta as M
import Cauterize.Dynamic
import Control.Exception

runTest :: O.TestOptions -> IO ()
runTest O.TestOptions { O.specName = sn, O.metaName = mn } = do
  Right s <- S.parseFile sn
  Right m <- M.parseFile mn

  pdp s $ CautType { ctName = "u8" , ctDetails = CDBuiltIn (BDu8 1) }
  pdp s $ CautType { ctName = "count" , ctDetails = CDSynonym (BDu8 1) }

  pdpE s $ CautType { ctName = "count" , ctDetails = CDBuiltIn (BDu8 1) }
  pdpE s $ CautType { ctName = "count" , ctDetails = CDSynonym (BDu16 1) }
  where
    pdp s t = print $ dynamicPack s t
    pdpE s t = pdp s t `catch` handleEx

    handleEx (TypeMisMatch s) = putStrLn $ "EXCEPTION: " ++ s
