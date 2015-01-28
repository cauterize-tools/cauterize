{-# LANGUAGE DeriveDataTypeable #-}
module Cauterize.Meta.Types
  ( Meta(..)
  , MetaType(..)
  , metaFromSpec
  , bytesRequired
  ) where

import Cauterize.Specification as Sp
import Cauterize.Common.Types
import Cauterize.FormHash
import Data.Word
import Data.Data
import qualified Data.List as L
import qualified Data.ByteString as B

data Meta = Meta { metaName :: Name
                 , metaVersion :: Integer
                 , metaTypeLength :: Integer
                 , metaDataLength :: Integer
                 , metaSpecHash :: FormHash

                 , metaSpecVersion :: Version

                 -- Types are guaranteed to be sorted based on their prefix bytes.
                 , metaTypes :: [MetaType]
                 } deriving (Show, Eq, Data, Typeable)

data MetaType = MetaType { metaTypeName :: Name
                         , metaTypePrefix :: [Word8]
                         } deriving (Show, Eq, Data, Typeable)


metaFromSpec :: Spec -> Meta
metaFromSpec (Spec name version hash (RangeSize _ smax) _ types) =
  Meta { metaName = name
       , metaVersion = 0
       , metaTypeLength = fromIntegral typePrefixLength
       , metaDataLength = bytesRequired $ fromIntegral typePrefixLength + fromIntegral smax
       , metaSpecVersion = version
       , metaSpecHash = hash
       , metaTypes = mts'
       }
  where
    typePrefixes = uniquePrefixes $ map (B.unpack . hashToByteString . spHash) types
    mts = case typePrefixes of
              Just l -> zipWith MetaType (map typeName types) l
              Nothing -> error "No unique prefixes!"
    mts' = L.sortBy (\(MetaType _ a) (MetaType _ b) -> a `compare` b) mts
    typePrefixLength = case typePrefixes of
                          Just (p:_) -> length p
                          _ -> error "Need at least one prefix to determine a prefix length"

bytesRequired :: Word64 -> Integer
bytesRequired i | (0          <= i) && (i < 256) = 1
                | (256        <= i) && (i < 65536) = 2
                | (25536      <= i) && (i < 4294967296) = 4
                | (4294967296 <= i) && (i <= 18446744073709551615) = 8
                | otherwise = error $ "Cannot express value: " ++ show i

uniquePrefixes :: Eq a => [[a]] -> Maybe [[a]]
uniquePrefixes ls = let count = length ls
                    in case dropWhile (\l -> length l < count) $ map L.nub $ L.transpose $ map L.inits ls of
                          [] -> Nothing
                          l -> (Just . head) l

