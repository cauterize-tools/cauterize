module Cauterize.FormHash where

import qualified Crypto.Hash.SHA1 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Numeric

data FormHash = FormHash B.ByteString
  deriving (Eq, Ord)

hashFn :: C.Ctx -> String -> C.Ctx
hashFn ctx s = ctx `C.update` BC.pack s

finalize :: C.Ctx -> FormHash
finalize = FormHash . C.finalize

class Hashable a where
  formHash :: a ->  FormHash
  formHash = finalize . formHashCtx

  formHashCtx :: a -> C.Ctx
  formHashCtx = formHashWith C.init

  formHashWith :: C.Ctx -> a -> C.Ctx

instance Show FormHash where
  show (FormHash bs) = concatMap showByte $ B.unpack bs
    where
      showByte b = case showHex b "" of
                    [x,y] -> [toUpper x, toUpper y]
                    [x]   -> ['0', toUpper x]
                    _     -> error "This should be impossible."

instance Hashable FormHash where
  formHashWith ctx (FormHash b) = ctx `C.update` b
