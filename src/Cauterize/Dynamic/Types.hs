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
import qualified Data.Text.Lazy as T
import qualified Cauterize.Specification as S

data CautType =
  CautType { ctName :: T.Text
           , ctDetails :: CautDetails
           }
  deriving (Show, Ord, Eq)

data CautDetails
  = CDBuiltIn BIDetails
  | CDSynonym BIDetails
  | CDArray { cdArrayElems :: [CautDetails] }
  | CDVector { cdVectorelems :: [CautDetails] }
  | CDRecord { cdRecordFields :: M.Map T.Text FieldValue }
  | CDCombination { cdCombinationFields :: M.Map T.Text FieldValue }
  | CDUnion { cdUnionFieldName :: T.Text, cdUnionFieldDetails :: FieldValue }
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
  deriving (Show, Ord, Eq, Data, Typeable)

data FieldValue = DataField CautDetails
                | EmptyField
  deriving (Show, Ord, Eq, Data, Typeable)

type TyMap = M.Map T.Text S.SpType

data Exceptions = TypeMisMatch { tmmExpected :: T.Text, tmmActual :: T.Text }
                | PrototypeMisMatch { ptmmTypeName :: T.Text, ptmmDetailType :: T.Text }
                | IncorrectArrayLength { ialExpected :: Integer, ialActual :: Integer }
                | IncorrectVectorLength { ivlMaximum :: Integer, ivlActual :: Integer }
                | InvalidType { invType :: T.Text }
                | InvalidTagForRepresentation { invTag :: Integer, invRepresentation :: T.Text }
                | InvalidLengthForLengthWidth { ilflwLength :: Integer, ilflwWidth :: Integer }
                | InvalidLengthWidth { ilwWidth :: Integer }
                | NotATagType { invTagType :: T.Text }
                | MissingField { mfField :: T.Text }
                | UnexpectedFields { ufFields :: [T.Text] }
                | UnexpectedDataField { udfField :: T.Text, udfData :: CautDetails }
                | UnexpectedEmptyField { udfField :: T.Text }
  deriving (Show, Eq, Data, Typeable)

instance Exception Exceptions
