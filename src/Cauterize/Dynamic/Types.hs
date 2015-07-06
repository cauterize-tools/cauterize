{-# LANGUAGE DeriveDataTypeable #-}
module Cauterize.Dynamic.Types
  ( CautType(..)
  , CautDetails(..)
  , PrimDetails(..)
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
import qualified Cauterize.CommonTypes as C

data CautType =
  CautType { ctName :: C.Identifier
           , ctDetails :: CautDetails
           }
  deriving (Show, Ord, Eq)

data CautDetails
  = CDSynonym { cdSynonymRef :: CautDetails }
  | CDRange { cdRangeValue :: Integer }
  | CDArray { cdArrayElems :: [CautDetails] }
  | CDVector { cdVectorElems :: [CautDetails] }
  | CDEnumeration { cdEnumVal :: C.Identifier }
  | CDRecord { cdRecordFields :: M.Map C.Identifier FieldValue }
  | CDCombination { cdCombinationFields :: M.Map C.Identifier FieldValue }
  | CDUnion { cdUnionFieldName :: C.Identifier, cdUnionFieldDetails :: FieldValue }
  deriving (Show, Ord, Eq, Data, Typeable)

data PrimDetails = PDu8 Word8
                 | PDu16 Word16
                 | PDu32 Word32
                 | PDu64 Word64
                 | PDs8 Int8
                 | PDs16 Int16
                 | PDs32 Int32
                 | PDs64 Int64
                 | PDf32 Float
                 | PDf64 Double
                 | PDbool Bool
  deriving (Show, Ord, Eq, Data, Typeable)

data FieldValue = DataField CautDetails
                | EmptyField
  deriving (Show, Ord, Eq, Data, Typeable)

type TyMap = M.Map C.Identifier S.Type

data Exceptions = TypeMisMatch { tmmExpected :: C.Identifier, tmmActual :: C.Identifier }
                | PrototypeMisMatch { ptmmTypeName :: C.Identifier, ptmmDetailType :: T.Text }
                | IncorrectArrayLength { ialExpected :: Integer, ialActual :: Integer }
                | IncorrectVectorLength { ivlMaximum :: Integer, ivlActual :: Integer }
                | RangeOutOfBounds { robMin :: Integer, robMax :: Integer, robValue :: Integer }
                | RangeDecodeOutOfBounds { rdobOffset :: C.Offset, rdobLength :: C.Length, rdobValue :: Integer }
                | InvalidType { invType :: C.Identifier }
                | InvalidTagForRepresentation { invTag :: Integer, invRepresentation :: T.Text }
                | InvalidLengthForLengthWidth { ilflwLength :: Integer, ilflwWidth :: Integer }
                | InvalidLengthWidth { ilwWidth :: Integer }
                | InvalidEnumerable { ieName :: C.Identifier }
                | NotATagType { invTagType :: T.Text }
                | MissingField { mfField :: C.Identifier }
                | UnexpectedFields { ufFields :: [C.Identifier] }
                | UnexpectedDataField { udfField :: C.Identifier, udfData :: CautDetails }
                | UnexpectedEmptyField { udfField :: C.Identifier }
  deriving (Show, Eq, Data, Typeable)

instance Exception Exceptions
