module Cauterize.FormHash where

import qualified Crypto.Hash.SHA256 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Numeric

data FormHash = FormHash B.ByteString

hashFn :: C.Ctx -> String -> C.Ctx
hashFn ctx s = ctx `C.update` BC.pack s

class Hashable a where
  formHash :: a ->  FormHash
  formHash a = FormHash $ C.finalize $ C.init `formHashWith` a

  formHashWith :: C.Ctx -> a -> C.Ctx

instance Show FormHash where
  show (FormHash bs) = concatMap showByte $ B.unpack bs
    where
      showByte b = case showHex b "" of
                    [x,y] -> [toUpper x, toUpper y]
                    [x]   -> ['0', toUpper x]
                    _     -> error "This should be impossible."
