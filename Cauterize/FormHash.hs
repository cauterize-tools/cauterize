module Cauterize.FormHash where

import qualified Crypto.Hash.SHA1 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Numeric

import Text.PrettyPrint
import Text.PrettyPrint.Class

data FormHash = FormHash B.ByteString
  deriving (Eq, Ord)

type HashContext = C.Ctx

hashInit :: C.Ctx
hashInit = C.init

hashFn :: C.Ctx -> String -> C.Ctx
hashFn ctx s = ctx `C.update` BC.pack s

finalize :: C.Ctx -> FormHash
finalize = FormHash . C.finalize

-- NOTE! This class should *NOT* be used by anyone dependent on the Cauterize
-- libraries. It is used simply to help compute different portions of the
-- Cauterize type tree. IT'S ALMOST NEVER THE CASE THAT A HASH COMPUTED WITH
-- THIS CLASS WILL END UP IN THE FINAL RENDERED SPECIFICATION. DO NOT PRETEND
-- OTHERWISE.
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

instance Hashable a => Hashable [a] where
  formHashWith = foldl formHashWith

instance Pretty FormHash where
  pretty = text . show
