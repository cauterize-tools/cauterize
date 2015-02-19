module Cauterize.Test.Test
  ( runTest
  ) where

import qualified Cauterize.Test.Test.Options as O
import qualified Cauterize.Specification as Spec
-- import qualified Cauterize.Meta as Meta
import qualified Data.Map as M
import qualified Data.ByteString as B
import Cauterize.Dynamic
import Control.Exception

runTest :: O.TestOptions -> IO ()
runTest O.TestOptions { O.specName = sn, O.metaName = _ } = do
  Right s <- Spec.parseFile sn
  -- Right m <- Meta.parseFile mn

  putStrLn "PUT ############################################################################"
  testPut s

  putStrLn "GET ############################################################################"
  testGet s

  where
    pdp s t = putStrLn $ "OK " ++ (show . B.unpack) (dynamicPack s t)
    pdpE s t = pdp s t `catch` handleEx

    pdu s n bs = case dynamicUnpack s (B.pack bs) n of
                  Left err -> putStrLn $ "ERROR " ++ err
                  Right res -> putStrLn $ "OK " ++ show res

    handleEx (TypeMisMatch s) = putStrLn $ "EXCEPTION type mismatch: " ++ s
    handleEx (IncorrectArrayLength s) = putStrLn $ "EXCEPTION incorrect array length: " ++ s
    handleEx (IncorrectVectorLength s) = putStrLn $ "EXCEPTION incorrect vector length: " ++ s
    handleEx (InvalidType s) = putStrLn $ "EXCEPTION invalid type: " ++ s
    handleEx (InvalidTag s) = putStrLn $ "EXCEPTION invalid tag: " ++ s
    handleEx (MissingField s) = putStrLn $ "EXCEPTION missing field: " ++ s
    handleEx (UnexpectedField s) = putStrLn $ "EXCEPTION unexpected field: " ++ s
    handleEx (UnexpectedDataField s) = putStrLn $ "EXCEPTION unexpected data field: " ++ s
    handleEx (UnexpectedEmptyField s) = putStrLn $ "EXCEPTION unexpected empty field: " ++ s

    testPut s = do
      pdp s CautType { ctName = "u8" , ctDetails = CDBuiltIn (BDu8 1) }
      pdp s CautType { ctName = "a_u8" , ctDetails = CDSynonym (BDu8 1) }
      pdp s CautType { ctName = "array_of_a_u8"
                     , ctDetails = CDArray { cdArrayElems = [ CDSynonym (BDu8 0)
                                                            , CDSynonym (BDu8 1)
                                                            , CDSynonym (BDu8 2)
                                                            ] } }
      pdp s CautType { ctName = "array_of_array_of_a_u8",
                       ctDetails = CDArray { cdArrayElems =
                          [ CDArray { cdArrayElems = [ CDSynonym (BDu8 100)
                                                     , CDSynonym (BDu8 101)
                                                     , CDSynonym (BDu8 102) ] }
                          , CDArray { cdArrayElems = [ CDSynonym (BDu8 103)
                                                     , CDSynonym (BDu8 104)
                                                     , CDSynonym (BDu8 105) ] }
                          , CDArray { cdArrayElems = [ CDSynonym (BDu8 106)
                                                     , CDSynonym (BDu8 107)
                                                     , CDSynonym (BDu8 108) ] }
                          ] } }

      pdp s CautType { ctName = "vec_of_u32"
                     , ctDetails = CDVector { cdVectorelems =
                        [ CDBuiltIn (BDu32 150)
                        , CDBuiltIn (BDu32 151)
                        , CDBuiltIn (BDu32 152)
                        ] } }

      pdp s CautType { ctName = "vec_of_u32"
                     , ctDetails = CDVector { cdVectorelems = [] } }

      pdp s CautType { ctName = "rthings"
                     , ctDetails = CDRecord { cdRecordFields =
                        M.fromList [ ( "ix0", DataField $ CDBuiltIn (BDu8 0x0F) )
                                   , ( "ix1", DataField $ CDBuiltIn (BDu16 0x0FFF) )
                                   , ( "ix2", DataField $ CDBuiltIn (BDu32 0x0FFFFFFF) )
                                   , ( "ix3", DataField $ CDBuiltIn (BDu64 0x0FFFFFFFFFFFFFFF) )
                                   ] } }

      pdp s CautType { ctName = "cthings"
                     , ctDetails = CDCombination { cdCombinationFields =
                        M.fromList [ ( "ix3", DataField $ CDBuiltIn (BDu64 0x0FFFFFFFFFFFFFFF) )
                                   ] } }

      pdp s CautType { ctName = "cthings"
                     , ctDetails = CDCombination { cdCombinationFields =
                        M.fromList [ ( "ix3", DataField $ CDBuiltIn (BDu64 0x0FFFFFFFFFFFFFFF) )
                                   , ( "ix2", DataField $ CDBuiltIn (BDu32 0x0FFFFFFF) )
                                   ] } }

      pdp s CautType { ctName = "uthings"
                     , ctDetails = CDUnion { cdUnionFieldName = "ix0"
                                           , cdUnionFieldDetails = DataField $ CDBuiltIn ( BDu8 60 )
                                           } }

      pdp s CautType { ctName = "uthings"
                     , ctDetails = CDUnion { cdUnionFieldName = "ix1"
                                           , cdUnionFieldDetails = DataField $ CDBuiltIn ( BDu16 61 )
                                           } }

      pdp s CautType { ctName = "uthings"
                     , ctDetails = CDUnion { cdUnionFieldName = "ix2"
                                           , cdUnionFieldDetails = EmptyField
                                           } }

      -- a_u8 is not a builtin
      pdpE s CautType { ctName = "a_u8" , ctDetails = CDBuiltIn (BDu8 1) }

      -- u8 is not a synonym
      pdpE s CautType { ctName = "u8", ctDetails = CDSynonym (BDu8 1) }

      -- u8 is not an array
      pdpE s CautType { ctName = "u8", ctDetails = CDArray { cdArrayElems = [CDSynonym (BDu8 1)] } }

      -- a_u8 doesn't hold a u16
      pdpE s CautType { ctName = "a_u8" , ctDetails = CDSynonym (BDu16 1) }

      -- array_of_a_u8 holds 3 elements
      pdpE s CautType { ctName = "array_of_a_u8"
                      , ctDetails = CDArray { cdArrayElems = [ CDSynonym (BDu8 0) ] } }

      -- array_of_array_of_a_u8 holds three array_of_a_u8 types, not three u8 types
      pdpE s CautType { ctName = "array_of_array_of_a_u8"
                      , ctDetails = CDArray { cdArrayElems = [ CDBuiltIn (BDu8 0)
                                                             , CDBuiltIn (BDu8 1)
                                                             , CDBuiltIn (BDu8 2)
                                                               ] } }

      -- array_of_array_of_a_u8 holds a_u8 types, not u8 types
      pdpE s CautType { ctName = "array_of_array_of_a_u8",
                        ctDetails = CDArray { cdArrayElems =
                           [ CDBuiltIn (BDu8 100)
                           , CDBuiltIn (BDu8 101)
                           , CDBuiltIn (BDu8 102)
                           ] } }

      -- array_of_array_of_a_u8 holds array_of_a_u8 types not array_of_u16 types
      pdpE s CautType { ctName = "array_of_array_of_a_u8",
                        ctDetails = CDArray { cdArrayElems =
                           [ CDBuiltIn (BDu16 100)
                           , CDBuiltIn (BDu16 101)
                           , CDBuiltIn (BDu16 102)
                           ] } }

      -- vector is too long
      pdpE s CautType { ctName = "vec_of_u32"
                      , ctDetails = CDVector { cdVectorelems =
                         [ CDBuiltIn (BDu32 150)
                         , CDBuiltIn (BDu32 151)
                         , CDBuiltIn (BDu32 152)
                         , CDBuiltIn (BDu32 153) -- vector's max length is 3
                         ] } }

      -- record is missing a field
      pdpE s CautType { ctName = "rthings"
                      , ctDetails = CDRecord { cdRecordFields =
                         M.fromList [ ( "ix0", DataField $ CDBuiltIn (BDu8 0x0F) )
                                    -- missing ix1
                                    , ( "ix2", DataField $ CDBuiltIn (BDu32 0x0FFFFFFF) )
                                    , ( "ix3", DataField $ CDBuiltIn (BDu64 0x0FFFFFFFFFFFFFFF) )
                                    ] } }


      -- record has a field of the wrong type
      pdpE s CautType { ctName = "rthings"
                      , ctDetails = CDRecord { cdRecordFields =
                         M.fromList [ ( "ix0", DataField $ CDBuiltIn (BDu16 0x0F) ) -- wrong type
                                    , ( "ix1", DataField $ CDBuiltIn (BDu16 0x0FFF) )
                                    , ( "ix2", DataField $ CDBuiltIn (BDu32 0x0FFFFFFF) )
                                    , ( "ix3", DataField $ CDBuiltIn (BDu64 0x0FFFFFFFFFFFFFFF) )
                                    ] } }

      -- record has too many fields
      pdpE s CautType { ctName = "rthings"
                      , ctDetails = CDRecord { cdRecordFields =
                         M.fromList [ ( "ix0", DataField $ CDBuiltIn (BDu8  0x0F) )
                                    , ( "ix1", DataField $ CDBuiltIn (BDu16 0x0FFF) )
                                    , ( "ix2", DataField $ CDBuiltIn (BDu32 0x0FFFFFFF) )
                                    , ( "ix3", DataField $ CDBuiltIn (BDu64 0x0FFFFFFFFFFFFFFF) )
                                    , ( "bad", DataField $ CDBuiltIn (BDu64 0x0FFFFFFFFFFFFFFF) ) -- not in the specification
                                    ] } }

    testGet s = do
      pdu s "u8"
        [0]
      pdu s "u8"
        [255]
      pdu s "array_of_u16"
        [1,0
        ,2,0
        ,3,0]
      pdu s "array_of_array_of_a_u8"
        [100,101,102
        ,103,104,105
        ,106,107,108]
      pdu s "uthings"
        [2]
