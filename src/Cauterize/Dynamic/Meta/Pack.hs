module Cauterize.Dynamic.Meta.Pack
  ( dynamicMetaPack
  , dynamicMetaPackHeaderAndPayload
  ) where

import Cauterize.Dynamic.Meta.Types
import Cauterize.Dynamic.Pack
import Cauterize.Dynamic.Types
import Control.Exception
import Data.Serialize.Put
import qualified Cauterize.Specification as Spec
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Cauterize.FormHash as F

dynamicMetaPack :: Spec.Spec -> MetaType -> B.ByteString
dynamicMetaPack spec t =
  let (h, p) = dynamicMetaPackHeaderAndPayload spec t
  in h `B.append` p

dynamicMetaPackHeaderAndPayload :: Spec.Spec -> MetaType -> (B.ByteString, B.ByteString)
dynamicMetaPackHeaderAndPayload spec t =
  case tn `M.lookup` m of
    Nothing -> throw $ InvalidType tn
    Just ty ->
      let prefix = take (fromIntegral dl) $ F.hashToBytes $ Spec.spHash ty
          h = runPut $ do
                packLengthWithWidth (fromIntegral . B.length $ ctPacked) (fromIntegral dl)
                putByteString (B.pack prefix)
      in (h, ctPacked)
  where
    (Spec.LengthTagWidth dl) = Spec.specLengthTagWidth spec
    ct = unMetaType t
    tn = ctName ct
    m = Spec.specTypeMap spec
    ctPacked = dynamicPack spec ct

packLengthWithWidth :: Integer -- length to pack
                    -> Integer -- width to pack length into
                    -> Put
packLengthWithWidth len 1 | 0 <= len && len < 2^(8  :: Integer) = putWord8 (fromIntegral len)
packLengthWithWidth len 2 | 0 <= len && len < 2^(16 :: Integer) = putWord16le (fromIntegral len)
packLengthWithWidth len 4 | 0 <= len && len < 2^(32 :: Integer) = putWord32le (fromIntegral len)
packLengthWithWidth len 8 | 0 <= len && len < 2^(64 :: Integer) = putWord64le (fromIntegral len)
packLengthWithWidth len w = throw $ InvalidLengthForLengthWidth len w
