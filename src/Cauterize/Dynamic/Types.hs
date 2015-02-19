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
  deriving (Show, Ord, Eq)

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
  deriving (Show, Ord, Eq)

data FieldValue = DataField CautDetails
                | EmptyField
  deriving (Show, Ord, Eq)

type TyMap = M.Map String S.SpType

data Exceptions = TypeMisMatch String
                | IncorrectArrayLength String
                | IncorrectVectorLength String
                | InvalidType String
                | InvalidTag String
                | MissingField String
                | UnexpectedField String
                | UnexpectedDataField String
                | UnexpectedEmptyField String
  deriving (Show, Data, Typeable)

instance Exception Exceptions
