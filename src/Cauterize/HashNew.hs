module Cauterize.HashNew
  ( Hash(unHash)
  , mkHash
  , hashToHex
  ) where

import Numeric
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype Hash = Hash { unHash :: B.ByteString }
  deriving (Eq, Ord, Show)

mkHash :: T.Text -> Hash
mkHash t = let h = SHA1.init `SHA1.update` T.encodeUtf8 t
           in Hash (SHA1.finalize h)

hashToHex :: Hash -> T.Text
hashToHex (Hash bs) = T.concat $ map showByte (B.unpack bs)
  where
    showByte b = case showHex b "" of
                  [x,y] -> T.pack [x, y]
                  [x]   -> T.pack ['0', x]
                  _     -> error "This should be impossible."
