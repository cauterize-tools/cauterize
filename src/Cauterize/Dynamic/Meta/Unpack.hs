{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Dynamic.Meta.Unpack
  ( dynamicMetaUnpack
  , dynamicMetaUnpackHeader
  , dynamicMetaUnpackFromHeader
  ) where

import Cauterize.Dynamic.Types
import Cauterize.Dynamic.Meta.Types
import Cauterize.Dynamic.Unpack
import Control.Exception
import Control.Monad
import Data.Serialize.Get
import qualified Cauterize.Specification as Spec
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text.Lazy as T

dynamicMetaUnpackHeader :: Spec.Spec -> B.ByteString -> Either String (MetaHeader, B.ByteString)
dynamicMetaUnpackHeader spec b = runGetState p b 0
  where
    p = do
      let tagLen = fromIntegral . Spec.unTypeTagWidth . Spec.specTypeTagWidth $ spec
      let dataLen = fromIntegral . Spec.unLengthTagWidth . Spec.specLengthTagWidth $ spec

      len <- liftM fromIntegral (unpackLengthWithWidth dataLen)
      tag <- liftM B.unpack $ getByteString tagLen

      return $ MetaHeader len tag

dynamicMetaUnpackFromHeader :: Spec.Spec -> MetaHeader -> B.ByteString -> Either T.Text (MetaType, B.ByteString)
dynamicMetaUnpackFromHeader spec (MetaHeader _ tag) b = case runGetState p b 0 of
                                                          Right r -> Right r
                                                          Left e -> Left $ T.pack e
  where
    p = case tag `M.lookup` m of
          Nothing -> fail $ "Invalid tag: " ++ show tag
          Just ty -> liftM MetaType (dynamicUnpack' spec (Spec.typeName ty))
    m = Spec.specTypeTagMap spec

dynamicMetaUnpack :: Spec.Spec -> B.ByteString -> Either T.Text (MetaType, B.ByteString)
dynamicMetaUnpack spec b =
  case dynamicMetaUnpackHeader spec b of
    Left err -> Left $ "Failed to unpack meta header:" `T.append` T.pack err
    Right (mh, remainder) -> dynamicMetaUnpackFromHeader spec mh remainder

unpackLengthWithWidth :: Integer -> Get Integer
unpackLengthWithWidth 1 = liftM fromIntegral getWord8
unpackLengthWithWidth 2 = liftM fromIntegral getWord16le
unpackLengthWithWidth 4 = liftM fromIntegral getWord32le
unpackLengthWithWidth 8 = liftM fromIntegral getWord64le
unpackLengthWithWidth w = throw $ InvalidLengthWidth w
