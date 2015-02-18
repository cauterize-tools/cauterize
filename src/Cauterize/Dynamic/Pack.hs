module Cauterize.Dynamic.Pack
  ( dynamicPack
  ) where

import Cauterize.Dynamic.Common
import Cauterize.Dynamic.Types
import Data.Maybe
import Data.Serialize.IEEE754
import Data.Serialize.Put
import Data.Bits
import qualified Cauterize.Common.Types as C
import qualified Cauterize.Specification as S
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as Set

import Debug.Trace

dynamicPack :: S.Spec -> CautType -> B.ByteString
dynamicPack s (CautType { ctName = n, ctDetails = d }) =
  let m = S.specTypeMap s
  in runPut $ dynamicPackDetails m n d

dynamicPackDetails :: TyMap -> String -> CautDetails -> Put
dynamicPackDetails m n det =
  case det of
    CDBuiltIn bd -> dynamicPackBuiltIn m n bd
    CDSynonym bd -> dynamicPackSynonym m n bd
    CDArray es -> dynamicPackArray m n es
    CDVector es -> dynamicPackVector m n es
    CDRecord fs -> dynamicPackRecord m n fs
    CDCombination fs -> dynamicPackCombination m n fs
    _ -> error "UNHANDLED"

dynamicPackBuiltIn :: TyMap -> String -> BIDetails -> Put
dynamicPackBuiltIn _ n det =
  if not (n `isNameOf` det)
  then throwTM $ "(" ++ show det ++ ") is not a '" ++ n ++ "'."
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
  let t = checkedTypeLookup m n isSynonym "synonym"
      trn = show $ C.synonymRepr . S.unSynonym $ t
  in if isNameOf trn det
      then dynamicPackBuiltIn m trn det
      else throwTM $ concat [ "'" , n, "' expects a builtin of type '", trn
                            , "', but was given the following: (" ++ show det ++ ")."
                            ]

dynamicPackArray :: TyMap -> String -> [CautDetails] -> Put
dynamicPackArray m n elems =
  let t = checkedTypeLookup m n isArray "array"
      a = S.unArray t
      el = fromIntegral $ length elems
      al = C.arrayLen a
      etype = C.arrayRef a
  in if al /= el
       then throwIAL $ concat [ "'", n, "' expects a length of ", show al
                              , ", but was given a list of elements ", show el, " long."
                              ]
       else mapM_ (dynamicPackDetails m etype) elems

dynamicPackVector :: TyMap -> String -> [CautDetails] -> Put
dynamicPackVector m n elems =
  let t = checkedTypeLookup m n isVector "vector"
      v = S.unVector t
      el = fromIntegral $ length elems
      vl = C.vectorMaxLen v
      etype = C.vectorRef v
      vlr = S.unLengthRepr $ S.lenRepr t
  in if el > vl
       then throwIVL $ concat [ "'", n, "' expects a length less than ", show vl
                              , ", but was given a list of elements ", show el, " long."
                              ]
       else do dynamicPackTag vlr el
               mapM_ (dynamicPackDetails m etype) elems

dynamicPackRecord :: TyMap -> String -> M.Map String CautDetails -> Put
dynamicPackRecord m n fields = checkedDynamicFields fs fields go
  where
    t = checkedTypeLookup m n isRecord "record"
    r = S.unRecord t
    fs = C.unFields . C.recordFields $ r
    go fields' = mapM_ (dynamicPackRecordField m fields') fs

dynamicPackCombination :: TyMap -> String -> M.Map String CautDetails -> Put
dynamicPackCombination m n fields = checkedDynamicFields fs fields go
  where
    t = checkedTypeLookup m n isCombination "combination"
    c = S.unCombination t
    fs = C.unFields . C.combinationFields $ c
    tr = S.unFlagsRepr . S.flagsRepr $ t
    go fields' =
      let fm = fieldsToMap fs
          ixs = map (fromIntegral . C.fIndex . fromJust . (`M.lookup` fm)) (M.keys fields')
          ixbits = foldl setBit (0 :: Int) ixs
      in do dynamicPackTag tr (fromIntegral ixbits)
            mapM_ (dynamicPackCombinationField m fields') fs

-- Retrieve a type from the map while also ensuring that its type matches some
-- expected condition. If the type does not match an exception is thrown.
checkedTypeLookup :: TyMap -- the map of types from the schema
                  -> String -- the name of the type to check
                  -> (S.SpType -> Bool) -- a checking function: isArray, isRecord, etc
                  -> String -- a name to use for the expected type (THIS IS SUPER HACKY)
                  -> S.SpType
checkedTypeLookup m n checker expectedStr =
  let t = n `lu` m
  in if not (checker t)
        then throwTM $ concat ["'", n, "' does not name an instance of the expected prototype: '"
                              , expectedStr, "'."]
        else t

-- Validates that the dynamic fields are all found in the specification fields.
-- The passed function can assume that there are no extra fields. There may
-- still be *missing* fields.
checkedDynamicFields :: [C.Field] -- input fields to compare againts
                     -> M.Map String CautDetails -- the dynamic fields that need checking
                     -> (M.Map String CautDetails -> Put) -- a function to accept the checked dynamic fields
                     -> Put -- the result of the passed function
checkedDynamicFields fs dfs a =
  let fset = fieldNameSet fs
      dset = M.keysSet dfs
      diff = dset `Set.difference` fset -- the fields in dset that are not in fset
  in if Set.empty /= diff
      then throwUF $ "the following fields are not allowed: " ++ (show . Set.toList) diff
      else a dfs

-- There are 4 tag widths in Cauterize: 8, 16, 32, and 64 bits. This will pack
-- an Integer as if it was one of those tag variants. If the specified Integer
-- is out of range, an exception is thrown.
dynamicPackTag :: C.BuiltIn -> Integer -> Put
dynamicPackTag b v =
  case b of
    C.BIu8  | v >= 0 && v <= u8Max  -> putWord8    (fromIntegral v)
    C.BIu16 | v >= 0 && v <= u16Max -> putWord16le (fromIntegral v)
    C.BIu32 | v >= 0 && v <= u32Max -> putWord32le (fromIntegral v)
    C.BIu64 | v >= 0 && v <= u64Max -> putWord64le (fromIntegral v)
    _ -> throwInvTag $ "'" ++ show b ++ " cannot be used to represent the tag value " ++ show v ++ "."
  where
    u8Max, u16Max, u32Max, u64Max :: Integer
    u8Max = 2^(8 :: Integer) - 1
    u16Max = 2^(16 :: Integer) - 1
    u32Max = 2^(32 :: Integer) - 1
    u64Max = 2^(64 :: Integer) - 1

-- Insists that the dynamic field map is complete.
dynamicPackRecordField :: TyMap -> M.Map String CautDetails -> C.Field -> Put
dynamicPackRecordField _ _ C.EmptyField {} = return ()
dynamicPackRecordField tym fm C.Field { C.fName = n, C.fRef = r } =
  let det = fromMaybe (throwMF $ "Field map is missing field '" ++ n ++ "'.")
                      (n `M.lookup` fm)
  in dynamicPackDetails tym r det

-- Skips fields not present in the dynamic field map.
dynamicPackCombinationField :: TyMap -> M.Map String CautDetails -> C.Field -> Put
dynamicPackCombinationField _ _ C.EmptyField {} = return ()
dynamicPackCombinationField tym fm C.Field { C.fName = n, C.fRef = r } =
  case n `M.lookup` fm of
    Just det -> dynamicPackDetails tym r det
    Nothing -> return ()
