{-# LANGUAGE DeriveDataTypeable #-}
module Cauterize.Common.Types
  ( BuiltIn(..)
  , Name
  , Signature
  , Version
  , minimalExpression
  , minimalBitField
  , builtInSize

  , TBuiltIn(..)
  , TScalar(..)
  , TArray(..)
  , TVector(..)
  , TStruct(..)
  , TSet(..)
  , TEnum(..)

  , Fields(..)
  , Field(..)

  , refSig

  , References(..)

  , fieldsLength
  ) where

import Data.List
import Data.Maybe
import Data.Data
import qualified Data.Map as M

type Name = String
type Signature = String
type Version = String

data BuiltIn = BIu8 | BIu16 | BIu32 | BIu64
             | BIs8 | BIs16 | BIs32 | BIs64
             | BIf32 | BIf64
             | BIbool
             | BIcu8 | BIcu16 | BIcu32
  deriving (Enum, Bounded, Ord, Eq, Data, Typeable)

-- | Returns the smallest BuiltIn that is capable of representing the provided
-- value.
minimalExpression :: Integral a => a -> BuiltIn
minimalExpression v | 0 > v' && v' >= -128 = BIs8
                    | 0 > v' && v' >= -32768 = BIs16
                    | 0 > v' && v' >= -2147483648 = BIs32
                    | 0 > v' && v' >= -9223372036854775808 = BIs64
                    | 0 <= v' && v' < 256 = BIu8
                    | 0 <= v' && v' < 65535 = BIu16
                    | 0 <= v' && v' < 4294967295 = BIu32
                    | 0 <= v' && v' < 18446744073709551615 = BIu64
                    | otherwise = error
                       $ "Cannot express value '" ++ show v' ++ "' as a builtin."
  where
    v' = fromIntegral v :: Integer

minimalBitField :: Integral a => a -> BuiltIn
minimalBitField v | 0 <= v' && v' <= 8 = BIu8
                  | 0 <= v' && v' <= 16 = BIu16
                  | 0 <= v' && v' <= 32 = BIu32
                  | 0 <= v' && v' <= 64 = BIu64
                  | otherwise = error
                      $ "Cannot express '" ++ show v' ++ "' bits in a bitfield."
  where
    v' = fromIntegral v :: Integer

builtInSize :: BuiltIn -> Integer
builtInSize BIu8       = 1
builtInSize BIu16      = 2
builtInSize BIu32      = 4
builtInSize BIu64      = 8
builtInSize BIs8       = 1
builtInSize BIs16      = 2
builtInSize BIs32      = 4
builtInSize BIs64      = 8
builtInSize BIbool     = 1
builtInSize BIf32      = 4
builtInSize BIf64      = 8
builtInSize BIcu8      = 1
builtInSize BIcu16     = 2
builtInSize BIcu32     = 4

instance Show BuiltIn where
  show BIu8       = "u8"
  show BIu16      = "u16"
  show BIu32      = "u32"
  show BIu64      = "u64"
  show BIs8       = "s8"
  show BIs16      = "s16"
  show BIs32      = "s32"
  show BIs64      = "s64"
  show BIbool     = "bool"
  show BIf32      = "f32"
  show BIf64      = "f64"
  show BIcu8      = "cu8"
  show BIcu16     = "cu16"
  show BIcu32     = "cu32"

instance Read BuiltIn where
  readsPrec _ "u8"       = [ (BIu8, "") ]
  readsPrec _ "u16"      = [ (BIu16, "") ]
  readsPrec _ "u32"      = [ (BIu32, "") ]
  readsPrec _ "u64"      = [ (BIu64, "") ]
  readsPrec _ "s8"       = [ (BIs8, "") ]
  readsPrec _ "s16"      = [ (BIs16, "") ]
  readsPrec _ "s32"      = [ (BIs32, "") ]
  readsPrec _ "s64"      = [ (BIs64, "") ]
  readsPrec _ "bool"     = [ (BIbool, "") ]
  readsPrec _ "f32"      = [ (BIf32, "") ]
  readsPrec _ "f64"      = [ (BIf64, "") ]
  readsPrec _ "cu8"      = [ (BIcu8, "") ]
  readsPrec _ "cu16"     = [ (BIcu16, "") ]
  readsPrec _ "cu32"     = [ (BIcu32, "") ]
  readsPrec _ s = error $ "ERROR: \"" ++ s ++ "\" is not a BuiltIn."

data TBuiltIn = TBuiltIn { unTBuiltIn :: BuiltIn }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TBuiltIn where
  referencesOf _ = []


data TArray = TArray { arrayName :: Name
                     , arrayRef :: Name
                     , arrayLen :: Integer
                     }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TArray where
  referencesOf (TArray _ n _) = [n]


data TVector = TVector { vectorName :: Name
                       , vectorRef :: Name
                       , vectorMaxLen :: Integer
                       }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TVector where
  referencesOf (TVector _ n _) = [n]


data TScalar = TScalar { scalarName :: Name
                       , scalarRepr :: BuiltIn }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TScalar where
  referencesOf (TScalar _ b) = [show b]


data TStruct = TStruct { structName :: Name, structFields :: Fields }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TStruct where
  referencesOf (TStruct _ (Fields rs)) = nub $ mapMaybe refRef rs


data TEnum = TEnum { enumName :: Name, enumFields :: Fields }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TEnum where
  referencesOf (TEnum _ (Fields rs)) = nub $ mapMaybe refRef rs


data TSet = TSet { setName :: Name, setFields :: Fields }
  deriving (Show, Ord, Eq, Data, Typeable)

instance References TSet where
  referencesOf (TSet _ (Fields rs)) = nub $ mapMaybe refRef rs


data Fields = Fields { unFields :: [Field] }
  deriving (Show, Ord, Eq, Data, Typeable)

fieldsLength :: Fields -> Int
fieldsLength (Fields fs) = length fs

data Field = Field { fName :: Name , fRef :: Name , fIndex :: Integer }
           | EmptyField { fName :: Name, fIndex :: Integer }
  deriving (Show, Ord, Eq, Data, Typeable)

refSig :: M.Map Name Signature -> Field -> Signature
refSig _ (EmptyField n _) = concat ["(field ", n, ")"]
refSig sm (Field n m _) = concat ["(field ", n, " ", luSig m, ")"]
  where
    luSig na = fromJust $ na `M.lookup` sm

refRef :: Field -> Maybe Name
refRef (EmptyField _ _) = Nothing
refRef (Field _ n _) = Just n

class References a where
  referencesOf :: a -> [Name]
