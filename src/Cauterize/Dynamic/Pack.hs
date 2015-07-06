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
import qualified Cauterize.CommonTypes as C
import qualified Cauterize.Specification as S
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T

dynamicPack :: S.Specification -> CautType -> B.ByteString
dynamicPack s (CautType { ctName = n, ctDetails = d }) =
  let m = S.specTypeMap s
  in runPut $ dynamicPackDetails m n d

dynamicPackDetails :: TyMap -> C.Identifier -> CautDetails -> Put
dynamicPackDetails m n det =
  case det of
    CDSynonym bd -> dynamicPackSynonym m n bd
    CDRange v -> dynamicPackRange m n v
    CDArray es -> dynamicPackArray m n es
    CDVector es -> dynamicPackVector m n es
    CDRecord fs -> dynamicPackRecord m n fs
    CDEnumeration v -> dynamicPackEnumeration m n v
    CDCombination fs -> dynamicPackCombination m n fs
    CDUnion fn fd -> dynamicPackUnion m n fn fd

-- There are 4 tag widths in Cauterize: 8, 16, 32, and 64 bits. This will pack
-- an Integer as if it was one of those tag variants. If the specified Integer
-- is out of range, an exception is thrown.
dynamicPackTag :: C.Tag -> Integer -> Put
dynamicPackTag b v =
  case b of
    C.T1 | v >= 0 && v <= t8Max  -> putWord8    (fromIntegral v)
    C.T2 | v >= 0 && v <= t16Max -> putWord16le (fromIntegral v)
    C.T4 | v >= 0 && v <= t32Max -> putWord32le (fromIntegral v)
    C.T8 | v >= 0 && v <= t64Max -> putWord64le (fromIntegral v)
    _ -> throw $ InvalidTagForRepresentation v (T.pack . show $ b)
  where
    t8Max, t16Max, t32Max, t64Max :: Integer
    t8Max  = 2^(8  :: Integer) - 1
    t16Max = 2^(16 :: Integer) - 1
    t32Max = 2^(32 :: Integer) - 1
    t64Max = 2^(64 :: Integer) - 1

dynamicPackPrim :: C.Identifier -> PrimDetails -> Put
dynamicPackPrim n det =
  if not (n `elem` C.allPrimNames)
  then throw $ TypeMisMatch n (fromJust $ C.mkIdentifier $ show $ det)
  else case det of
         PDu8 d -> putWord8 d
         PDu16 d -> putWord16le d
         PDu32 d -> putWord32le d
         PDu64 d -> putWord64le d
         PDs8 d -> putWord8 $ fromIntegral d
         PDs16 d -> putWord16le $ fromIntegral d
         PDs32 d -> putWord32le $ fromIntegral d
         PDs64 d -> putWord64le $ fromIntegral d
         PDf32 d -> putFloat32le d
         PDf64 d -> putFloat64le d
         PDbool d -> putWord8 $ if d then 1 else 0

dynamicPackSynonym :: TyMap -> C.Identifier -> CautDetails -> Put
dynamicPackSynonym m n det =
  let (S.Type { S.typeDesc = t }) = checkedTypeLookup m n isSynonym "synonym"
  in dynamicPackDetails m (S.synonymRef t) det

dynamicPackRange :: TyMap -> C.Identifier -> Integer -> Put
dynamicPackRange m n v =
  let (S.Type { S.typeDesc = t }) = checkedTypeLookup m n isRange "range"
      rmin = fromIntegral $ S.rangeOffset t
      rmax = (fromIntegral $ S.rangeLength t) - (fromIntegral $ S.rangeOffset t)
  in if v < rmin || v > rmax
        then throw $ RangeOutOfBounds rmin rmax v
        else dynamicPackTag (S.rangeTag t) v

dynamicPackArray :: TyMap -> C.Identifier -> [CautDetails] -> Put
dynamicPackArray m n elems =
  let (S.Type { S.typeDesc = t }) = checkedTypeLookup m n isArray "array"
      el = fromIntegral $ length elems
      al = S.arrayLength t
      etype = S.arrayRef t
  in if al /= el
       then throw $ IncorrectArrayLength (fromIntegral al) (fromIntegral el)
       else mapM_ (dynamicPackDetails m etype) elems

dynamicPackVector :: TyMap -> C.Identifier -> [CautDetails] -> Put
dynamicPackVector m n elems =
  let (S.Type { S.typeDesc = t }) = checkedTypeLookup m n isVector "vector"
      el = length elems
      vl = S.vectorLength t
      etype = S.vectorRef t
      vt = S.vectorTag t
  in if fromIntegral el > vl
       then throw $ IncorrectVectorLength (fromIntegral vl) (fromIntegral el)
       else do dynamicPackTag vt (fromIntegral el)
               mapM_ (dynamicPackDetails m etype) elems

dynamicPackRecord :: TyMap -> C.Identifier -> M.Map C.Identifier FieldValue -> Put
dynamicPackRecord m n fields = checkedDynamicFields fs fields go
  where
    (S.Type { S.typeDesc = t }) = checkedTypeLookup m n isRecord "record"
    fs = S.recordFields t
    go fields' = mapM_ (dynamicPackRecordField m fields') fs

dynamicPackEnumeration  :: TyMap -> C.Identifier -> C.Identifier -> Put
dynamicPackEnumeration m n val = dynamicPackTag tag ix
  where
    (S.Type { S.typeDesc = t }) = checkedTypeLookup m n isEnumeration "enumeration"
    tag = S.enumerationTag t
    vals = let ev = S.enumerationValues t
           in zip (map S.enumValName ev) (map S.enumValIndex ev)
    ix = case val `lookup` vals of
            Just ix' -> ix'
            Nothing -> throw $ InvalidEnumerable val

dynamicPackCombination :: TyMap -> C.Identifier -> M.Map C.Identifier FieldValue -> Put
dynamicPackCombination m n fields = checkedDynamicFields fs fields go
  where
    (S.Type { S.typeDesc = t }) = checkedTypeLookup m n isCombination "combination"
    fs = S.combinationFields t
    ct = S.combinationTag t
    go fields' =
      let fm = fieldsToNameMap fs
          ixs = map (fromIntegral . S.fieldIndex . fromJust . (`M.lookup` fm)) (M.keys fields')
          ixbits = foldl setBit (0 :: Int) ixs
      in do dynamicPackTag ct (fromIntegral ixbits)
            mapM_ (dynamicPackCombinationField m fields') fs

dynamicPackUnion :: TyMap -> C.Identifier -> C.Identifier -> FieldValue -> Put
dynamicPackUnion m n fn fv = do
  dynamicPackTag fir (S.fieldIndex field)
  case (field, fv) of
    (S.EmptyField {}, EmptyField) -> return ()
    (S.EmptyField { S.fieldName = efn }, DataField d)  -> unexpectedData efn d
    (S.DataField  { S.fieldRef = r },    DataField fd) -> dynamicPackDetails m r fd
    (S.DataField  { S.fieldName = dfn }, EmptyField)   -> unexpectedEmpty dfn
  where
    (S.Type { S.typeDesc = t }) = checkedTypeLookup m n isUnion "union"
    fm = fieldsToNameMap . S.unionFields $ t
    fir = S.unionTag $ t
    field = fromMaybe (throw $ UnexpectedFields [fn])
                      (fn `M.lookup` fm)

-- Retrieve a type from the map while also ensuring that its type matches some
-- expected condition. If the type does not match an exception is thrown.
checkedTypeLookup :: TyMap -- the map of types from the schema
                  -> C.Identifier -- the name of the type to check
                  -> (S.Type -> Bool) -- a checking function: isArray, isRecord, etc
                  -> T.Text -- a name to use for the expected type (THIS IS SUPER HACKY)
                  -> S.Type
checkedTypeLookup m n checker expectedStr =
  let t = n `lu` m
  in if not (checker t)
        then throw $ PrototypeMisMatch n expectedStr
        else t

-- Validates that the dynamic fields are all found in the specification fields.
-- The passed function can assume that there are no extra fields. There may
-- still be *missing* fields.
checkedDynamicFields :: [S.Field] -- input fields to compare againts
                     -> M.Map C.Identifier FieldValue -- the dynamic fields that need checking
                     -> (M.Map C.Identifier FieldValue -> Put) -- a function to accept the checked dynamic fields
                     -> Put -- the result of the passed function
checkedDynamicFields fs dfs a =
  let fset = fieldNameSet fs
      dset = M.keysSet dfs
      diff = dset `Set.difference` fset -- the fields in dset that are not in fset
  in if Set.empty /= diff
      then throw $ UnexpectedFields (Set.toList diff)
      else a dfs

-- Insists that the dynamic field map is complete.
dynamicPackRecordField :: TyMap -> M.Map C.Identifier FieldValue -> S.Field -> Put
dynamicPackRecordField _ fm (S.EmptyField { S.fieldName = n }) = dynamicPackEmptyField fm n
dynamicPackRecordField tym fm (S.DataField { S.fieldName = n, S.fieldRef = r }) =
  let det = fromMaybe (throw $ MissingField n)
                      (n `M.lookup` fm)
  in case det of
    DataField det' -> dynamicPackDetails tym r det'
    EmptyField -> unexpectedEmpty n

-- Skips fields not present in the dynamic field map.
dynamicPackCombinationField :: TyMap -> M.Map C.Identifier FieldValue -> S.Field -> Put
dynamicPackCombinationField _ fm (S.EmptyField { S.fieldName = n }) =
  case n `M.lookup` fm of
    Just (DataField det) -> unexpectedData n det
    Just EmptyField -> dynamicPackEmptyField fm n
    Nothing -> return ()
dynamicPackCombinationField tym fm (S.DataField { S.fieldName = n, S.fieldRef = r }) =
  case n `M.lookup` fm of
    Just EmptyField -> unexpectedEmpty n
    Just (DataField det) -> dynamicPackDetails tym r det
    Nothing -> return ()

unexpectedEmpty :: C.Identifier -> c
unexpectedEmpty n = throw $ UnexpectedEmptyField n

unexpectedData :: C.Identifier -> CautDetails -> c
unexpectedData n d = throw $ UnexpectedDataField n d

dynamicPackEmptyField :: M.Map C.Identifier FieldValue -> C.Identifier -> Put
dynamicPackEmptyField fm n =
  let det = fromMaybe (throw $ MissingField n)
                      (n `M.lookup` fm)
  in case det of
      EmptyField -> return ()
      DataField d -> unexpectedData n d
