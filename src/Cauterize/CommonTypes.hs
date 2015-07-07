{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Cauterize.CommonTypes
  ( Offset
  , Length
  , Identifier
    , unIdentifier
    , unsafeMkIdentifier
    , mkIdentifier
  , Prim(..)
    , allPrims
    , allPrimNames
    , primToText
    , primToSize
    , primMap
    , primFittingAllInts
  , Tag(..)
    , tagToText
    , tagToSize
    , tagRequired
    , tagForBits
  , Size
    , sizeMin
    , sizeMax
    , mkSize
    , mkConstSize
  ) where

import Data.String
import Data.Word
import Data.Int
import Data.Char
import Data.Text (Text, pack, empty)
import Data.Maybe
import Data.Data

import qualified Data.Map as M

type Offset = Int64

type Length = Word64

newtype Identifier = Identifier { unIdentifier :: Text }
  deriving (Eq, Ord, Data, Typeable)

data Prim
  = PU8
  | PU16
  | PU32
  | PU64
  | PS8
  | PS16
  | PS32
  | PS64
  | PF32
  | PF64
  | PBool
  deriving (Show, Eq, Enum, Bounded)

data Tag = T1 | T2 | T4 | T8
  deriving (Show, Eq)

data Size = Size { sizeMin :: Integer, sizeMax :: Integer }

isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (s:r) = first && rest
  where
    first = isAsciiLower s
    rest = all (\c -> isAsciiLower c || isDigit c || ('_' == c)) r

unsafeMkIdentifier :: String -> Identifier
unsafeMkIdentifier s =
  fromMaybe
    (error $ "unsafeMkIdentifier: invalid input string \"" ++ s ++ "\"")
    (mkIdentifier s)

mkIdentifier :: String -> Maybe Identifier
mkIdentifier [] = Just (Identifier empty)
mkIdentifier i =
    if isValidIdentifier i
      then Just (Identifier $ pack i)
      else Nothing

mkSize :: Integer -> Integer -> Size
mkSize rmin rmax | rmin < 1 = error ("Min size less than 1: " ++ show rmin)
                 | rmax < 1 = error ("Max size less than 1: " ++ show rmax)
                 | rmin <= rmax = Size rmin rmax
                 | otherwise = error ("Bad min and max: min " ++ show rmin ++ " >= max " ++ show rmax ++ ".")

mkConstSize :: Integer -> Size
mkConstSize sz = mkSize sz sz

primToText :: Prim -> Identifier
primToText PU8    = "u8"
primToText PU16   = "u16"
primToText PU32   = "u32"
primToText PU64   = "u64"
primToText PS8    = "s8"
primToText PS16   = "s16"
primToText PS32   = "s32"
primToText PS64   = "s64"
primToText PF32   = "f32"
primToText PF64   = "f64"
primToText PBool  = "bool"

allPrims :: [Prim]
allPrims = [minBound..maxBound]

allPrimNames :: [Identifier]
allPrimNames = map primToText allPrims

primToSize :: Prim -> Size
primToSize PU8    = mkConstSize 1
primToSize PU16   = mkConstSize 2
primToSize PU32   = mkConstSize 4
primToSize PU64   = mkConstSize 8
primToSize PS8    = mkConstSize 1
primToSize PS16   = mkConstSize 2
primToSize PS32   = mkConstSize 4
primToSize PS64   = mkConstSize 8
primToSize PF32   = mkConstSize 4
primToSize PF64   = mkConstSize 8
primToSize PBool  = mkConstSize 1

primFittingAllInts :: [Integer] -> Prim
primFittingAllInts vs
    | not signed && all (<= w8max) vs  = PU8
    | not signed && all (<= w16max) vs = PU16
    | not signed && all (<= w32max) vs = PU32
    | not signed && all (<= w64max) vs = PU64

    | signed && all (\i -> i8min  <= i && i <= i8max)  vs = PS8
    | signed && all (\i -> i16min <= i && i <= i16max) vs = PS16
    | signed && all (\i -> i32min <= i && i <= i32max) vs = PS32
    | signed && all (\i -> i64min <= i && i <= i64max) vs = PS64

    | otherwise = error $ "Unable to express all values in a single primitive: " ++ show vs
  where
    signed = any (< 0) vs

    w64max = fromIntegral (maxBound :: Word64)
    w32max = fromIntegral (maxBound :: Word32)
    w16max = fromIntegral (maxBound :: Word16)
    w8max  = fromIntegral (maxBound :: Word8)

    i64max = fromIntegral (maxBound :: Int64)
    i32max = fromIntegral (maxBound :: Int32)
    i16max = fromIntegral (maxBound :: Int16)
    i8max  = fromIntegral (maxBound :: Int8)

    i64min = fromIntegral (minBound :: Int64)
    i32min = fromIntegral (minBound :: Int32)
    i16min = fromIntegral (minBound :: Int16)
    i8min  = fromIntegral (minBound :: Int8)

primMap :: M.Map Identifier Prim
primMap = M.fromList $ zip allPrimNames allPrims

tagToText :: Tag -> Identifier
tagToText T1 = "t1"
tagToText T2 = "t2"
tagToText T4 = "t4"
tagToText T8 = "t8"

tagToSize :: Tag -> Size
tagToSize T1 = mkConstSize 1
tagToSize T2 = mkConstSize 2
tagToSize T4 = mkConstSize 4
tagToSize T8 = mkConstSize 8

tagRequired :: Integral a => a -> Tag
tagRequired i | (0          <= i') && (i' < 256) = T1
              | (256        <= i') && (i' < 65536) = T2
              | (25536      <= i') && (i' < 4294967296) = T4
              | (4294967296 <= i') && (i' <= 18446744073709551615) = T8
              | otherwise = error $ "Cannot express tag for value: " ++ show i'
  where
    i' = fromIntegral i :: Integer

tagForBits :: Integral a => a -> Tag
tagForBits v | 0 <= v' && v' <= 8 = T1
             | 0 <= v' && v' <= 16 = T2
             | 0 <= v' && v' <= 32 = T4
             | 0 <= v' && v' <= 64 = T8
             | otherwise = error
                 $ "Cannot express '" ++ show v' ++ "' bits in a bitfield."
  where
    v' = fromIntegral v :: Integer

instance IsString Identifier where
  fromString s =
    fromMaybe
      (error $ "IsString Identifier: invalid input string \"" ++ s ++ "\"")
      (mkIdentifier s)

instance Show Identifier where
  show (Identifier i) = "i" ++ show i

instance Show Size where
  show (Size smin smax) | smin == smax = "Size " ++ show smax
                        | otherwise = "Size " ++ show smin ++ ".." ++ show smax
