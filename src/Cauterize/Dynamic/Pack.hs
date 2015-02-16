module Cauterize.Dynamic.Pack
  ( dynamicPack
  ) where

import Cauterize.Dynamic.Common
import Cauterize.Dynamic.Types
import Data.Serialize.IEEE754
import Data.Serialize.Put
import qualified Cauterize.Common.Types as C
import qualified Cauterize.Specification as S
import qualified Data.ByteString as B

import Debug.Trace

dynamicPack :: S.Spec -> CautType -> B.ByteString
dynamicPack s (CautType { ctName = n, ctDetails = d }) =
  let m = S.specTypeMap s
      b = case d of
            CDBuiltIn bd -> dynamicPackBuiltIn m n bd
            CDSynonym bd -> dynamicPackSynonym m n bd
            CDArray es -> dynamicPackArray m n es
            _ -> error "UNHANDLED"
  in runPut b

dynamicPackBuiltIn :: TyMap -> String -> BIDetails -> Put
dynamicPackBuiltIn _ n det =
  if not (n `isNameOf` det)
  then throwTM $ "Type mismatch: (" ++ show det ++ ") is not a '" ++ n ++ "'."
  else case det of
         BDu8 d -> putWord8 d
         BDu16 d -> putWord16le d
         BDu32 d -> putWord32le d
         BDu64 d -> putWord64le d
         BDs8 d -> putWord8 $ fromIntegral d
         BDs16 d -> putWord16le $ fromIntegral d
         BDs32 d -> putWord32le $ fromIntegral d
         BDs64 d -> putWord32le $ fromIntegral d
         BDf32 d -> putFloat32le d
         BDf64 d -> putFloat64le d
         BDbool d -> putWord8 $ if d then 1 else 0
         BDcu8 d -> putWord8 d
         BDcu16 d -> putWord16le d
         BDcu32 d -> putWord32le d

dynamicPackSynonym :: TyMap -> String -> BIDetails -> Put
dynamicPackSynonym m n det =
  let trn = show $ C.synonymRepr . S.unSynonym $ n `lu` m
  in if isNameOf trn det
      then dynamicPackBuiltIn m trn det
      else throwTM $ concat [ "Type mismatch: '"
                            , n, "' expects a builtin of type '", trn
                            , "', but was given the following: (" ++ show det ++ ")."
                            ]

dynamicPackArray :: TyMap -> String -> [CautDetails] -> Put
dynamicPackArray = undefined
