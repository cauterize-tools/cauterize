{-# LANGUAGE DeriveDataTypeable #-}
module Cauterize.Dynamic.Types
  ( CautType(..)
  , CautDetails(..)
  , BIDetails(..)
  , FieldValue(..)
  , TyMap
  , Exceptions(..)
  ) where

import Control.Exception
import Data.Data
import Data.Int
import Data.Word
import qualified Data.Map as M
import qualified Cauterize.Specification as S

data CautType =
  CautType { ctName :: String
           , ctDetails :: CautDetails
           }
  deriving (Show, Ord, Eq)

data CautDetails
  = CDBuiltIn BIDetails
  | CDSynonym BIDetails
  | CDArray { cdArrayElems :: [CautDetails] }
  | CDVector { cdVectorelems :: [CautDetails] }
  | CDRecord { cdRecordFields :: M.Map String FieldValue }
  | CDCombination { cdCombinationFields :: M.Map String FieldValue }
  | CDUnion { cdUnionFieldName :: String, cdUnionFieldDetails :: FieldValue }
  deriving (Show, Ord, Eq, Data, Typeable)

data BIDetails = BDu8 Word8
               | BDu16 Word16
               | BDu32 Word32
               | BDu64 Word64
               | BDs8 Int8
               | BDs16 Int16
               | BDs32 Int32
               | BDs64 Int64
               | BDf32 Float
               | BDf64 Double
               | BDbool Bool
               | BDcu8 Word8
               | BDcu16 Word16
               | BDcu32 Word32
  deriving (Show, Ord, Eq, Data, Typeable)

data FieldValue = DataField CautDetails
                | EmptyField
  deriving (Show, Ord, Eq, Data, Typeable)

type TyMap = M.Map String S.SpType

data Exceptions = TypeMisMatch { tmmExpected :: String, tmmActual :: String }
                | PrototypeMisMatch { ptmmExpectedType :: String, ptmmDetailType :: String }
                | IncorrectArrayLength { ialExpected :: Integer, ialActual :: Integer }
                | IncorrectVectorLength { ivlMaximum :: Integer, ivlActual :: Integer }
                | InvalidType { invType :: String }
                | InvalidTagForRepresentation { invTag :: Integer, invRepresentation :: String }
                | InvalidLengthForLengthWidth { ilflwLength :: Integer, ilflwWidth :: Integer }
                | InvalidLengthWidth { ilwWidth :: Integer }
                | NotATagType { invTagType :: String }
                | MissingField { mfField :: String }
                | UnexpectedFields { ufFields :: [String] }
                | UnexpectedDataField { udfField :: String, udfData :: CautDetails }
                | UnexpectedEmptyField { udfField :: String }
  deriving (Show, Eq, Data, Typeable)

instance Exception Exceptions
