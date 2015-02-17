module Cauterize.Test.Test
  ( runTest
  ) where

import qualified Cauterize.Test.Test.Options as O
import qualified Cauterize.Specification as S
import qualified Cauterize.Meta as M
import Cauterize.Dynamic
import Control.Exception

runTest :: O.TestOptions -> IO ()
runTest O.TestOptions { O.specName = sn, O.metaName = mn } = do
  Right s <- S.parseFile sn
  Right m <- M.parseFile mn

  pdp s $ CautType { ctName = "u8" , ctDetails = CDBuiltIn (BDu8 1) }
  pdp s $ CautType { ctName = "a_u8" , ctDetails = CDSynonym (BDu8 1) }
  pdp s $ CautType { ctName = "array_of_a_u8"
                   , ctDetails = CDArray { cdArrayElems = [ CDSynonym (BDu8 0)
                                                          , CDSynonym (BDu8 1)
                                                          , CDSynonym (BDu8 2)
                                                          ] } }
  pdp s $ CautType { ctName = "array_of_array_of_a_u8",
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

  -- a_u8 is not a builtin
  pdpE s $ CautType { ctName = "a_u8" , ctDetails = CDBuiltIn (BDu8 1) }

  -- u8 is not a synonym
  pdpE s $ CautType { ctName = "u8", ctDetails = CDSynonym (BDu8 1) }

  -- a_u8 doesn't hold a u16
  pdpE s $ CautType { ctName = "a_u8" , ctDetails = CDSynonym (BDu16 1) }

  -- array_of_a_u8 holds 3 elements
  pdpE s $ CautType { ctName = "array_of_a_u8"
                    , ctDetails = CDArray { cdArrayElems = [ CDSynonym (BDu8 0) ] } }

  -- array_of_array_of_a_u8 holds three array_of_a_u8 types, not three u8 types
  pdpE s $ CautType { ctName = "array_of_array_of_a_u8"
                    , ctDetails = CDArray { cdArrayElems = [ CDBuiltIn (BDu8 0)
                                                           , CDBuiltIn (BDu8 1)
                                                           , CDBuiltIn (BDu8 2)
                                                           ] } }

  -- array_of_array_of_a_u8 holds a_u8 types, not u8 types
  pdpE s $ CautType { ctName = "array_of_array_of_a_u8",
                      ctDetails = CDArray { cdArrayElems =
                         [ CDBuiltIn (BDu8 100)
                         , CDBuiltIn (BDu8 101)
                         , CDBuiltIn (BDu8 102)
                         ] } }

  -- array_of_array_of_a_u8 holds array_of_a_u8 types not array_of_u16 types
  pdpE s $ CautType { ctName = "array_of_array_of_a_u8",
                      ctDetails = CDArray { cdArrayElems =
                         [ CDBuiltIn (BDu16 100)
                         , CDBuiltIn (BDu16 101)
                         , CDBuiltIn (BDu16 102)
                         ] } }
  where
    pdp s t = putStrLn $ "OK " ++ (show $ dynamicPack s t)
    pdpE s t = pdp s t `catch` handleEx

    handleEx (TypeMisMatch s) = putStrLn $ "EXCEPTION type mismatch: " ++ s
    handleEx (IncorrectArrayLength s) = putStrLn $ "EXCEPTION incorrect array length: " ++ s
    handleEx (InvalidType s) = putStrLn $ "EXCEPTION invalid type: " ++ s
