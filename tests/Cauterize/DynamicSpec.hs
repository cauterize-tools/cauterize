{-# LANGUAGE OverloadedStrings #-}
module Cauterize.DynamicSpec
  ( spec
  ) where

import Cauterize.Dynamic

import Test.Hspec
import TestSupport

import Control.Exception
import Data.Word
import qualified Cauterize.Specification as Spec
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text.Lazy as T

-- First, a few utilities.
packCompare :: Spec.Spec -> CautType -> [Word8] -> Expectation
packCompare s t b = dynamicPack s t `shouldBe` B.pack b

unpackCompare :: Spec.Spec -> CautType -> [Word8] -> Expectation
unpackCompare s t b = case dynamicUnpack s (ctName t) (B.pack b) of
                          Right res -> res `shouldBe` t
                          Left err -> expectationFailure (T.unpack err)

-- packShouldThrow is defined in terms of packCompare because of how laziness
-- interacts with exceptions. Namely, that often times we'll never force the
-- path that results in the exception.
--
-- I'm sure there's a better way, but this was expedient.
packShouldThrow :: Exception e => Spec.Spec -> CautType -> Selector e -> Expectation
packShouldThrow s t e = packCompare s t [] `shouldThrow` e

-- Next, the specs
spec :: Spec
spec = do
  describe "dynamic encoding" dynamicEncoding
  describe "dynamic encoding exceptions" dynamicEncodingExceptions
  describe "dynamic decoding" dynamicDecoding

dynamicEncoding :: Spec
dynamicEncoding = do
  itWithSpec "should encode u8 types correctly" $ \specification ->
    let t = CautType { ctName = "u8"
                     , ctDetails = CDBuiltIn (BDu8 1) }
        b = [1]
    in packCompare specification t b

  itWithSpec "should encode s8 types correctly" $ \specification ->
    let t = CautType { ctName = "s8"
                     , ctDetails = CDBuiltIn (BDs8 (-2)) }
        b = [254]
    in packCompare specification t b

  -- Floating point types are annoying. 1.0/32.0 is expressable exactly in
  -- IEEE754, so we use that for now.
  itWithSpec "should encode f32 types correctly" $ \specification ->
    let t = CautType { ctName = "f32"
                     , ctDetails = CDBuiltIn (BDf32 (1.0/32.0)) }
        b = [0,0,0,61]
    in packCompare specification t b

  itWithSpec "should encode synonym types correctly" $ \specification ->
    let t = CautType { ctName = "a_u8"
                     , ctDetails = CDSynonym (BDu8 2) }
        b = [2]
    in packCompare specification t b

  itWithSpec "should encode array types correctly" $ \specification ->
    let t = CautType { ctName = "array_of_a_u8"
                     , ctDetails = CDArray { cdArrayElems = [ CDSynonym (BDu8 0)
                                                            , CDSynonym (BDu8 1)
                                                            , CDSynonym (BDu8 2) ] } }
        b = [0,1,2]
    in packCompare specification t b

  itWithSpec "should encode vector types correctly" $ \specification ->
    let t = CautType { ctName = "vector_of_a_u16"
                     , ctDetails = CDVector { cdVectorelems = [ CDSynonym (BDu16 0)
                                                              , CDSynonym (BDu16 1) ] } }
        b = [2, 0, 0, 1, 0]
    in packCompare specification t b

  itWithSpec "should encode record types correctly" $ \specification ->
    let t = CautType { ctName = "record_of_unsigned"
                     , ctDetails = CDRecord { cdRecordFields =
                        M.fromList [ ( "rec_u8",  DataField $ CDBuiltIn (BDu8 1) )
                                   , ( "rec_u16", DataField $ CDBuiltIn (BDu16 10) )
                                   , ( "rec_u32", DataField $ CDBuiltIn (BDu32 100) )
                                   , ( "rec_u64", DataField $ CDBuiltIn (BDu64 1000) ) ] } }
        b = [ 1 -- u8
            , 10, 0 -- u16
            , 100, 0, 0, 0 -- u32
            , 0xE8, 0x03, 0, 0, 0, 0, 0, 0 -- u64
            ]
    in packCompare specification t b

  itWithSpec "should encode combination types correctly" $ \specification ->
    let t = CautType { ctName = "combination_of_signed"
                     , ctDetails = CDCombination { cdCombinationFields =
                        M.fromList [ ( "rec_s8", DataField $ CDBuiltIn (BDs8 (5)) )
                                   , ( "rec_s64", DataField $ CDBuiltIn (BDs64 (10)) )
                                   ] } }
        b = [ 0x09 -- flags
            , 5 -- s8
            , 10, 0, 0, 0, 0, 0, 0, 0 -- s64
            ]
    in packCompare specification t b

  itWithSpec "should encode union types correctly" $ \specification ->
    let t = CautType { ctName = "union_of_float"
                     , ctDetails = CDUnion { cdUnionFieldName = "uni_f32"
                                           , cdUnionFieldDetails = DataField $ CDBuiltIn ( BDf32 (1.0/32.0) ) } }
        b = [0, 0, 0, 0, 61]
    in packCompare specification t b

dynamicEncodingExceptions :: Spec
dynamicEncodingExceptions = do
  itWithSpec "knows synonyms are not builtins" $ \specification ->
    let t = CautType { ctName = "a_u8" , ctDetails = CDBuiltIn (BDu8 1) }
    in packShouldThrow specification t (== TypeMisMatch "a_u8" "BDu8 1")

  itWithSpec "knows builtins are not synonyms" $ \specification ->
    let t = CautType { ctName = "u8" , ctDetails = CDSynonym (BDu8 1) }
    in packShouldThrow specification t (== PrototypeMisMatch "u8" "synonym")

  itWithSpec "knows arrays are not vectors" $ \specification ->
    let t = CautType { ctName = "array_of_a_u8" , ctDetails = CDVector [] }
    in packShouldThrow specification t (== PrototypeMisMatch "array_of_a_u8" "vector")

  itWithSpec "knows u8 type don't hold u16 values" $ \specification ->
    let t = CautType { ctName = "u8" , ctDetails = CDBuiltIn (BDu16 30) }
    in packShouldThrow specification t (== TypeMisMatch "u8" "BDu16 30")

  itWithSpec "knows how many elements an array holds" $ \specification ->
    let t = CautType { ctName = "array_of_a_u8" , ctDetails = CDArray [] }
    in packShouldThrow specification t (== IncorrectArrayLength 3 0)

  itWithSpec "knows how many elements a vector holds" $ \specification ->
    let t = CautType { ctName = "vector_of_a_u16"
                     , ctDetails = CDVector { cdVectorelems = [ CDBuiltIn (BDu16 0)
                                                              , CDBuiltIn (BDu16 1)
                                                              , CDBuiltIn (BDu16 2)
                                                              , CDBuiltIn (BDu16 3)
                                                              ] } }
    in packShouldThrow specification t (== IncorrectVectorLength 3 4)

  itWithSpec "knows when records are missing a field" $ \specification ->
    let t = CautType { ctName = "record_of_unsigned"
                     , ctDetails = CDRecord { cdRecordFields =
                        M.fromList [ ( "rec_u8",  DataField $ CDBuiltIn (BDu8 1) )
                                   , ( "rec_u16", DataField $ CDBuiltIn (BDu16 10) )
                                   , ( "rec_u64", DataField $ CDBuiltIn (BDu64 1000) ) ] } }
    in packShouldThrow specification t (== MissingField "rec_u32")

  itWithSpec "knows when a field has the wrong type" $ \specification ->
    let t = CautType { ctName = "record_of_unsigned"
                     , ctDetails = CDRecord { cdRecordFields =
                        M.fromList [ ( "rec_u8",  DataField $ CDBuiltIn (BDu16 1) ) -- should be u8
                                   , ( "rec_u16", DataField $ CDBuiltIn (BDu16 10) )
                                   , ( "rec_u32", DataField $ CDBuiltIn (BDu32 100) )
                                   , ( "rec_u64", DataField $ CDBuiltIn (BDu64 1000) ) ] } }
    in packShouldThrow specification t (== TypeMisMatch "u8" "BDu16 1")

  itWithSpec "knows when a field has too many fields" $ \specification ->
    let t = CautType { ctName = "record_of_unsigned"
                     , ctDetails = CDRecord { cdRecordFields =
                        M.fromList [ ( "rec_u8",  DataField $ CDBuiltIn (BDu16 1) ) -- should be u8
                                   , ( "rec_u16", DataField $ CDBuiltIn (BDu16 10) )
                                   , ( "rec_u32", DataField $ CDBuiltIn (BDu32 100) )
                                   , ( "rec_u64", DataField $ CDBuiltIn (BDu64 1000) )
                                   , ( "rec_u64_2", DataField $ CDBuiltIn (BDu64 10000) ) ] } }
    in packShouldThrow specification t (== UnexpectedFields ["rec_u64_2"])


dynamicDecoding :: Spec
dynamicDecoding = do
  describe "dynamic decoding" $ do
    itWithSpec "can decode u8 values" $ \specification ->
      let t = CautType { ctName = "u8", ctDetails = CDBuiltIn (BDu8 100) }
          b = [100]
      in unpackCompare specification t b

    itWithSpec "can decode u16 values" $ \specification ->
      let t = CautType { ctName = "u16", ctDetails = CDBuiltIn (BDu16 65534) }
          b = [254, 255]
      in unpackCompare specification t b

    itWithSpec "can decode array values" $ \specification ->
      let t = CautType { ctName = "array_of_a_u8"
                       , ctDetails = CDArray { cdArrayElems = [ CDSynonym (BDu8 1)
                                                              , CDSynonym (BDu8 10)
                                                              , CDSynonym (BDu8 100)
                                                              ] } }
          b = [1, 10, 100]
      in unpackCompare specification t b

    it "can decode vectors" pending
    it "can decode records" pending
    it "can decode combinations" pending
    it "can decode unions" pending
