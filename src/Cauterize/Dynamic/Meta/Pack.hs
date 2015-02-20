module Cauterize.Dynamic.Meta.Pack
  ( dynamicMetaPack
  ) where

import Cauterize.Dynamic.Common
import Cauterize.Dynamic.Meta.Types
import Cauterize.Dynamic.Pack
import Cauterize.Dynamic.Types
import Data.Serialize.Put
import qualified Cauterize.Meta as Meta
import qualified Cauterize.Specification as Spec
import qualified Data.ByteString as B
import qualified Data.Map as M

dynamicMetaPack :: Spec.Spec -> Meta.Meta -> MetaType -> B.ByteString
dynamicMetaPack spec meta t =
  case tn `M.lookup` m of
    Nothing -> throwInvType $ "The type '" ++ tn ++ "' does not appear in the meta description."
    Just (Meta.MetaType { Meta.metaTypePrefix = p }) -> runPut $ do
      packLengthWithWidth (fromIntegral . B.length $ ctPacked) dl
      putByteString (B.pack p)
      putByteString ctPacked
  where
    dl = Meta.metaDataLength meta
    ct = unMetaType t
    tn = ctName ct
    m = Meta.metaTypeMap meta
    ctPacked = dynamicPack spec ct

packLengthWithWidth :: Integer -- length to pack
                    -> Integer -- width to pack length into
                    -> Put
packLengthWithWidth len 1 | 0 <= len && len < 2^(8  :: Integer) = putWord8 (fromIntegral len)
packLengthWithWidth len 2 | 0 <= len && len < 2^(16 :: Integer) = putWord16le (fromIntegral len)
packLengthWithWidth len 4 | 0 <= len && len < 2^(32 :: Integer) = putWord32le (fromIntegral len)
packLengthWithWidth len 8 | 0 <= len && len < 2^(64 :: Integer) = putWord64le (fromIntegral len)
packLengthWithWidth len w = throwInvTag $ "Cannot represent the length '" ++ show len ++ "' with a width of '" ++ show w ++ "'."
