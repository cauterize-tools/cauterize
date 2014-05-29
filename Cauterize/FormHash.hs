module Cauterize.FormHash
  ( FormHash(..)
  , HashContext

  , hashInit
  , hashUpdate
  , hashString
  , hashFinalize
  ) where

import qualified Crypto.Hash.SHA1 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Numeric

import Text.PrettyPrint
import Text.PrettyPrint.Class

data FormHash = FormHash { hashToByteString :: B.ByteString }
  deriving (Eq, Ord)

type HashContext = C.Ctx

hashInit :: HashContext
hashInit = C.init

hashUpdate :: HashContext -> String -> HashContext
hashUpdate ctx s = ctx `C.update` BC.pack s

hashString :: String -> FormHash
hashString = hashFinalize . hashUpdate hashInit

hashFinalize :: HashContext -> FormHash
hashFinalize = FormHash . C.finalize

instance Show FormHash where
  show (FormHash bs) = concatMap showByte $ B.unpack bs
    where
      showByte b = case showHex b "" of
                    [x,y] -> [toUpper x, toUpper y]
                    [x]   -> ['0', toUpper x]
                    _     -> error "This should be impossible."

instance Pretty FormHash where
  pretty f = parens $ text "sha1" <+> (text . show $ f)

