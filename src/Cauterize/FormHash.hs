{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Cauterize.FormHash
  ( FormHash(..)
  , HashContext

  , hashInit
  , hashUpdate
  , hashText
  , hashToText
  , hashFinalize

  , hashToBytes
  ) where

import qualified Crypto.Hash.SHA1 as C
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import Data.Data
import Data.Word
import Numeric

import Text.PrettyPrint.Leijen.Text

data FormHash = FormHash { hashToByteString :: B.ByteString }
  deriving (Eq, Ord, Data, Typeable)

type HashContext = C.Ctx

hashInit :: HashContext
hashInit = C.init

hashUpdate :: HashContext -> T.Text -> HashContext
hashUpdate ctx s = ctx `C.update` TE.encodeUtf8 (T.toStrict s)

hashText :: T.Text -> FormHash
hashText = hashFinalize . hashUpdate hashInit

hashFinalize :: HashContext -> FormHash
hashFinalize = FormHash . C.finalize

hashToBytes :: FormHash -> [Word8]
hashToBytes (FormHash h) = B.unpack h

hashToText :: FormHash -> T.Text
hashToText (FormHash bs) = T.concat $ map showByte $ B.unpack bs
    where
      showByte b = case showHex b "" of
                    [x,y] -> T.pack [x, y]
                    [x]   -> T.pack ['0', x]
                    _     -> error "This should be impossible."

instance Pretty FormHash where
  pretty f = parens $ "sha1" <+> (text . hashToText) f

instance Show FormHash where
  show = T.unpack . hashToText
