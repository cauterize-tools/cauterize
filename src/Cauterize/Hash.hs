module Cauterize.Hash
  ( Hash(unHash)
  , mkHash
  , mkHashFromHexString
  , hashToHex
  , hashToBytes
  , hashNull
  ) where

import Numeric
import Data.Word (Word8)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype Hash = Hash { unHash :: B.ByteString }
  deriving (Eq, Ord)

mkHash :: T.Text -> Hash
mkHash t = let h = SHA1.init `SHA1.update` T.encodeUtf8 t
           in Hash (SHA1.finalize h)

mkHashFromHexString :: T.Text -> Hash
mkHashFromHexString t | T.length t == 40 && (`elem` validChars) `T.all` t = go
                      | otherwise = error $ "Unable to create hash. String wrong length ("
                                         ++ show (T.length t)
                                         ++ "): "
                                         ++ T.unpack t
  where
    validChars = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
    go = Hash . B.pack $ map (fromHex . T.unpack) (T.chunksOf 2 t)
    fromHex hbyte = case readHex hbyte of
                      [(n,"")] -> n
                      _ -> error $ "mkHashFromHexString: Unable to read hex byte '" ++ hbyte ++ "'."


hashToHex :: Hash -> T.Text
hashToHex (Hash bs) = T.concat $ map showByte (B.unpack bs)
  where
    showByte b = case showHex b "" of
                  [x,y] -> T.pack [x, y]
                  [x]   -> T.pack ['0', x]
                  _     -> error "hashToHex: This should be impossible."

hashToBytes :: Hash -> [Word8]
hashToBytes (Hash b) = B.unpack b

hashNull :: Hash
hashNull = Hash $ B.pack (replicate 20 0)

instance Show Hash where
  show h = "SHA1:" ++ T.unpack (hashToHex h)

