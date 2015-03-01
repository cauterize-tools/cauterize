module Cauterize.Test.Generate
  ( printArbSpec
  ) where

import Cauterize.Generate
import Text.PrettyPrint.Class
import qualified Cauterize.Schema as SC
import qualified Cauterize.Test.Generate.Options as OPT

printArbSpec :: OPT.GenerateOpts -> IO ()
printArbSpec (OPT.GenerateOpts typeCount encSize prototypes) = outputCaut prototypes typeCount encSize

outputCaut :: [PrototypeVariant] -- which prototypes to allow
           -> Integer -- number of types in the schema
           -> Integer -- maximum encoded size of the schema
           -> IO ()
outputCaut ps tc es = do
  s <- generateSchemaWith tc es 0.95 ps
  case SC.checkSchema s of
    [] -> print . pretty $ s
    errors -> error $ "ERROR: " ++ show errors
