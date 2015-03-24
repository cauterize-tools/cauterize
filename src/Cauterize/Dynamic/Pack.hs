{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Dynamic.Pack
  ( dynamicPack
  ) where

import Cauterize.Dynamic.Common
import Cauterize.Dynamic.Types
import Control.Exception
import Data.Maybe
import Data.Serialize.IEEE754
import Data.Serialize.Put
import Data.Bits
import qualified Cauterize.Common.Types as C
import qualified Cauterize.Specification as S
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T

dynamicPack :: S.Spec -> CautType -> B.ByteString
dynamicPack s (CautType { ctName = n, ctDetails = d }) =
  let m = S.specTypeMap s
  in runPut $ dynamicPackDetails m n d

dynamicPackDetails :: TyMap -> T.Text -> CautDetails -> Put
dynamicPackDetails m n det =
  case det of
    CDBuiltIn bd -> dynamicPackBuiltIn m n bd
    CDSynonym bd -> dynamicPackSynonym m n bd
    CDArray es -> dynamicPackArray m n es
    CDVector es -> dynamicPackVector m n es
    CDRecord fs -> dynamicPackRecord m n fs
    CDCombination fs -> dynamicPackCombination m n fs
    CDUnion fn fd -> dynamicPackUnion m n fn fd

dynamicPackBuiltIn :: TyMap -> T.Text -> BIDetails -> Put
dynamicPackBuiltIn _ n det =
  if not (n `isNameOf` det)
  then throw $ TypeMisMatch n (T.pack . show $ det)
  else case det of
         BDu8 d -> putWord8 d
         BDu16 d -> putWord16le d
         BDu32 d -> putWord32le d
         BDu64 d -> putWord64le d
         BDs8 d -> putWord8 $ fromIntegral d
         BDs16 d -> putWord16le $ fromIntegral d
         BDs32 d -> putWord32le $ fromIntegral d
         BDs64 d -> putWord64le $ fromIntegral d
         BDf32 d -> putFloat32le d
         BDf64 d -> putFloat64le d
         BDbool d -> putWord8 $ if d then 1 else 0

dynamicPackSynonym :: TyMap -> T.Text -> BIDetails -> Put
dynamicPackSynonym m n det =
  let t = checkedTypeLookup m n isSynonym "synonym"
      trn = T.pack . show $ C.synonymRepr . S.unSynonym $ t
  in if isNameOf trn det
      then dynamicPackBuiltIn m trn det
      else throw $ TypeMisMatch trn (T.pack . show $ det)

dynamicPackArray :: TyMap -> T.Text -> [CautDetails] -> Put
dynamicPackArray m n elems =
  let t = checkedTypeLookup m n isArray "array"
      a = S.unArray t
      el = fromIntegral $ length elems
      al = C.arrayLen a
      etype = C.arrayRef a
  in if al /= el
       then throw $ IncorrectArrayLength al el
       else mapM_ (dynamicPackDetails m etype) elems

dynamicPackVector :: TyMap -> T.Text -> [CautDetails] -> Put
dynamicPackVector m n elems =
  let t = checkedTypeLookup m n isVector "vector"
      v = S.unVector t
      el = fromIntegral $ length elems
      vl = C.vectorMaxLen v
      etype = C.vectorRef v
      vlr = S.unLengthRepr $ S.lenRepr t
  in if el > vl
       then throw $ IncorrectVectorLength vl el
       else do dynamicPackTag vlr el
               mapM_ (dynamicPackDetails m etype) elems

dynamicPackRecord :: TyMap -> T.Text -> M.Map T.Text FieldValue -> Put
dynamicPackRecord m n fields = checkedDynamicFields fs fields go
  where
    t = checkedTypeLookup m n isRecord "record"
    r = S.unRecord t
    fs = C.unFields . C.recordFields $ r
    go fields' = mapM_ (dynamicPackRecordField m fields') fs

dynamicPackCombination :: TyMap -> T.Text -> M.Map T.Text FieldValue -> Put
dynamicPackCombination m n fields = checkedDynamicFields fs fields go
  where
    t = checkedTypeLookup m n isCombination "combination"
    c = S.unCombination t
    fs = C.unFields . C.combinationFields $ c
    tr = S.unFlagsRepr . S.flagsRepr $ t
    go fields' =
      let fm = fieldsToNameMap fs
          ixs = map (fromIntegral . C.fIndex . fromJust . (`M.lookup` fm)) (M.keys fields')
          ixbits = foldl setBit (0 :: Int) ixs
      in do dynamicPackTag tr (fromIntegral ixbits)
            mapM_ (dynamicPackCombinationField m fields') fs

dynamicPackUnion :: TyMap -> T.Text -> T.Text -> FieldValue -> Put
dynamicPackUnion m n fn fv = do
  dynamicPackTag fir (C.fIndex field)
  case (field, fv) of
    (C.EmptyField {}, EmptyField) -> return ()
    (C.EmptyField { C.fName = efn }, DataField d) -> unexpectedData efn d
    (C.Field { C.fRef = r }, DataField fd) -> dynamicPackDetails m r fd
    (C.Field { C.fName = dfn }, EmptyField) -> unexpectedEmpty dfn
  where
    t = checkedTypeLookup m n isUnion "union"
    u = S.unUnion t
    fm = fieldsToNameMap . C.unFields . C.unionFields $ u
    fir = S.unTagRepr . S.tagRepr $ t
    field = fromMaybe (throw $ UnexpectedFields [fn])
                      (fn `M.lookup` fm)

-- Retrieve a type from the map while also ensuring that its type matches some
-- expected condition. If the type does not match an exception is thrown.
checkedTypeLookup :: TyMap -- the map of types from the schema
                  -> T.Text -- the name of the type to check
                  -> (S.SpType -> Bool) -- a checking function: isArray, isRecord, etc
                  -> T.Text -- a name to use for the expected type (THIS IS SUPER HACKY)
                  -> S.SpType
checkedTypeLookup m n checker expectedStr =
  let t = n `lu` m
  in if not (checker t)
        then throw $ PrototypeMisMatch n expectedStr
        else t

-- Validates that the dynamic fields are all found in the specification fields.
-- The passed function can assume that there are no extra fields. There may
-- still be *missing* fields.
checkedDynamicFields :: [C.Field] -- input fields to compare againts
                     -> M.Map T.Text FieldValue -- the dynamic fields that need checking
                     -> (M.Map T.Text FieldValue -> Put) -- a function to accept the checked dynamic fields
                     -> Put -- the result of the passed function
checkedDynamicFields fs dfs a =
  let fset = fieldNameSet fs
      dset = M.keysSet dfs
      diff = dset `Set.difference` fset -- the fields in dset that are not in fset
  in if Set.empty /= diff
      then throw $ UnexpectedFields (Set.toList diff)
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
    _ -> throw $ InvalidTagForRepresentation v (T.pack . show $ b)
  where
    u8Max, u16Max, u32Max, u64Max :: Integer
    u8Max = 2^(8 :: Integer) - 1
    u16Max = 2^(16 :: Integer) - 1
    u32Max = 2^(32 :: Integer) - 1
    u64Max = 2^(64 :: Integer) - 1

-- Insists that the dynamic field map is complete.
dynamicPackRecordField :: TyMap -> M.Map T.Text FieldValue -> C.Field -> Put
dynamicPackRecordField _ fm (C.EmptyField { C.fName = n }) = dynamicPackEmptyField fm n
dynamicPackRecordField tym fm (C.Field { C.fName = n, C.fRef = r }) =
  let det = fromMaybe (throw $ MissingField n)
                      (n `M.lookup` fm)
  in case det of
    DataField det' -> dynamicPackDetails tym r det'
    EmptyField -> unexpectedEmpty n

-- Skips fields not present in the dynamic field map.
dynamicPackCombinationField :: TyMap -> M.Map T.Text FieldValue -> C.Field -> Put
dynamicPackCombinationField _ fm (C.EmptyField { C.fName = n }) =
  case n `M.lookup` fm of
    Just (DataField det) -> unexpectedData n det
    Just EmptyField -> dynamicPackEmptyField fm n
    Nothing -> return ()
dynamicPackCombinationField tym fm (C.Field { C.fName = n, C.fRef = r }) =
  case n `M.lookup` fm of
    Just EmptyField -> unexpectedEmpty n
    Just (DataField det) -> dynamicPackDetails tym r det
    Nothing -> return ()

unexpectedEmpty :: T.Text -> c
unexpectedEmpty n = throw $ UnexpectedEmptyField n

unexpectedData :: T.Text -> CautDetails -> c
unexpectedData n d = throw $ UnexpectedDataField n d

dynamicPackEmptyField :: M.Map T.Text FieldValue -> T.Text -> Put
dynamicPackEmptyField fm n =
  let det = fromMaybe (throw $ MissingField n)
                      (n `M.lookup` fm)
  in case det of
      EmptyField -> return ()
      DataField d -> unexpectedData n d
