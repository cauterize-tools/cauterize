module Cauterize.Dynamic.Meta.Unpack
  ( dynamicMetaUnpack
  ) where

import Cauterize.Dynamic.Common
import Cauterize.Dynamic.Meta.Types
import Cauterize.Dynamic.Unpack
import Control.Monad
import Data.Serialize.Get
import qualified Cauterize.Meta as Meta
import qualified Cauterize.Specification as Spec
import qualified Data.ByteString as B
import qualified Data.Map as M

dynamicMetaUnpack :: Spec.Spec -> Meta.Meta -> B.ByteString -> Either String MetaType
dynamicMetaUnpack spec meta b = flip runGet b $ do
  _ <- unpackLengthWithWidth (fromIntegral $ Meta.metaDataLength meta)
  tag <- liftM B.unpack $ getByteString (fromIntegral $ Meta.metaTypeLength meta)
  case tag `M.lookup` m of
    Nothing -> throwInvTag $ "Invalid meta tag: " ++ show tag
    Just (Meta.MetaType { Meta.metaTypeName = n }) -> liftM MetaType (dynamicUnpack' spec n)
  where
    m = Meta.metaTagMap meta


unpackLengthWithWidth :: Integer -> Get Integer
unpackLengthWithWidth 1 = liftM fromIntegral getWord8
unpackLengthWithWidth 2 = liftM fromIntegral getWord16le
unpackLengthWithWidth 4 = liftM fromIntegral getWord32le
unpackLengthWithWidth 8 = liftM fromIntegral getWord64le
unpackLengthWithWidth w = throwInvTag $ "Cannot represent a length that is '" ++ show w ++ "' bytes wide."
