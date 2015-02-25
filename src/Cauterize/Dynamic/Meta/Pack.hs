module Cauterize.Dynamic.Meta.Pack
  ( dynamicMetaPack
  , dynamicMetaPackHeaderAndPayload
  ) where

import Cauterize.Dynamic.Meta.Types
import Cauterize.Dynamic.Pack
import Cauterize.Dynamic.Types
import Control.Exception
import Data.Serialize.Put
import qualified Cauterize.Meta as Meta
import qualified Cauterize.Specification as Spec
import qualified Data.ByteString as B
import qualified Data.Map as M

dynamicMetaPack :: Spec.Spec -> Meta.Meta -> MetaType -> B.ByteString
dynamicMetaPack spec meta t =
  let (h, p) = dynamicMetaPackHeaderAndPayload spec meta t
  in h `B.append` p

dynamicMetaPackHeaderAndPayload :: Spec.Spec -> Meta.Meta -> MetaType -> (B.ByteString, B.ByteString)
dynamicMetaPackHeaderAndPayload spec meta t =
  case tn `M.lookup` m of
    Nothing -> throw $ InvalidType tn
    Just (Meta.MetaType { Meta.metaTypePrefix = p }) ->
      let h = runPut $ do
                packLengthWithWidth (fromIntegral . B.length $ ctPacked) dl
                putByteString (B.pack p)
      in (h, ctPacked)
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
packLengthWithWidth len w = throw $ InvalidLengthForLengthWidth len w
