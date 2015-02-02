module Cauterize.Test.Generate
  ( printArbSpec
  ) where

import qualified Cauterize.Test.Generate.Options as OPT

import Cauterize.Schema.Arbitrary
import qualified Cauterize.Schema as SC

import Test.QuickCheck.Gen
import Text.PrettyPrint.Class
import qualified Data.Set as S

printArbSpec :: OPT.GenerateOpts -> IO ()
printArbSpec (OPT.GenerateOpts count) = outputCaut OPT.allProtoParams count

outputCaut :: S.Set ProtoParam -> Int -> IO ()
outputCaut ps c = do
  s <- mkASchema ps c
  case s of
    Left es -> print es
    Right s' -> print . pretty $ s'

{- TODO: When dealing with arrays, it's likely that the maximum depth we should
 - use is 5. Possibly 4. The size of arrays is exponential and this tends to
 - make huge huge huge huge huge types. -}
mkASchema :: S.Set ProtoParam -> Int -> IO (Either String SC.Schema)
mkASchema ps c = do
  s <- generate $ arbSchemaParam ps c
  case SC.checkSchema s of
    [] -> return (Right s)
    es -> return $ Left (show es)
