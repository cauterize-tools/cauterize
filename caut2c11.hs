module Main where

import Options.Applicative
import Text.PrettyPrint as P hiding ((<>))

import Data.Word
import Data.Char
import Data.List
import Data.List.Split
import Numeric

import Cauterize.FormHash
import Cauterize.Specification as SP
import Cauterize.Common.Primitives
import Cauterize.Common.Types
-- import Cauterize.Common.IndexedRef

-- import Paths_cauterize

data Caut2C11Opts = Caut2C11Opts
  { inputFile :: String
  , outputDirectory :: String
  } deriving (Show)

optParser :: Parser Caut2C11Opts
optParser = Caut2C11Opts
  <$> strOption
    ( long "input"
   <> metavar "FILE_PATH"
   <> help "Input Cauterize specification file."
    )
  <*> strOption
    ( long "output"
   <> metavar "DIRECTORY_PATH"
   <> help "Output Cauterize directory."
    )

options :: ParserInfo Caut2C11Opts
options = info (optParser <**> helper)
            ( fullDesc
           <> progDesc "Process Cauterize schema files."
            )

runWithOptions :: (Caut2C11Opts -> IO ()) -> IO ()
runWithOptions fn = execParser options >>= fn

main :: IO ()
main = runWithOptions caut2c11

caut2c11 :: Caut2C11Opts -> IO ()
caut2c11 opts = do
  s <- SP.parseFile (inputFile opts)
  case s of
    Left e -> print e
    Right s' -> putStrLn $ renderHFile s'

renderHFile :: Spec Name -> String
renderHFile spec = render $ vcat [ gaurdOpen
                                 , gaurdDef
                                 , blankLine
                                 , includes
                                 , blankLine
                                 , docLines
                                 , blankLine
                                 , libDefines
                                 , blankLine
                                 , libHash
                                 , blankLine
                                 , typeHashes
                                 , blankLine
                                 , typeSizes
                                 , blankLine
                                 , constDecls
                                 , blankLine
                                 , fwdDecls
                                 , blankLine
                                 , gaurdClose
                                 ]
  where
    gaurdOpen  = text $ "#ifndef _CAUTERIZE_CAUT2C11_" ++ libName ++ "_"
    gaurdDef   = text $ "#define _CAUTERIZE_CAUT2C11_" ++ libName ++ "_"
    gaurdClose = text $ "#endif /* _CAUTERIZE_CAUT2C11_" ++ libName ++ "_ */"
    includes = vcat [ libInclude "stddef.h"
                    , libInclude "stdint.h"
                    , libInclude "stdbool.h"
                    , blankLine
                    , relInclude "cauterize.h"
                    ]

    docLines = vcat [ text   "/*"
                    , text $ " * Name: " ++ libName
                    , text $ " * Version: " ++ libVersion
                    , text   " */"
                    ]

    libDefines = vcat [ text $ "#define NAME_" ++ libName ++ " \"" ++ libName ++ "\""
                      , text $ "#define VERSION_" ++ libName ++ " \"" ++ libVersion ++ "\""
                      , text $ "#define MIN_SIZE_" ++ libName ++ " (" ++ (show . SP.minSize . SP.specSize $ spec) ++ ")"
                      , text $ "#define MAX_SIZE_" ++ libName ++ " (" ++ (show . SP.maxSize . SP.specSize $ spec) ++ ")"
                      ]

    libHash = text $ "uint8_t const SCHEMA_HASH_" ++ libName ++ "[] = \n  " ++ hashBytes ++ ";"

    typeHashes = vcat $ map typeHash sTypes
    typeSizes = vcat $ concatMap typeSize sTypes
    constDecls = vcat $ map constDecl sTypes
    fwdDecls = vcat $ map (text . fwdDecl) sTypes

    typeHash t = text $ ( "uint8_t const TYPE_HASH_" ++ libName ++ "_" ++ typeName t ++ "[] =\n  ")
                     ++ (bytesToCArray . hashToBytes $ SP.spHash t)
                     ++ "\n"

    typeSize t = [ text $ "#define MIN_SIZE_" ++ libName ++ "_" ++ typeName t ++ " (" ++ (show . minSize $ t) ++ ")"
                 , text $ "#define MAX_SIZE_" ++ libName ++ "_" ++ typeName t ++ " (" ++ (show . maxSize $ t) ++ ")"
                 , blankLine
                 ]

    constDecl ty = let n = typeName ty
                       constDec = constStr n
                   in case ty of
                        SP.BuiltIn {} -> P.empty
                        SP.Scalar {} -> P.empty
                        SP.Const t _ _ -> constDec "VALUE" (show . constValue $ t)
                        SP.FixedArray t _ _ -> constDec "LENGTH" (show . fixedArrLen $ t)
                        SP.BoundedArray t _ _ _ -> constDec "MAX_LENGTH" (show . boundedArrMaxLen $ t)
                        SP.Struct {} -> P.empty
                        SP.Set {} -> P.empty
                        SP.Enum {} -> P.empty
                        SP.Partial {} -> P.empty
                        SP.Pad {} -> P.empty

    fullConstName tyName term = "CONST_" ++ libName ++ "_" ++ tyName ++ "_" ++ term
    constStr tyName term val = text $ fullConstName tyName term ++ " (" ++ val ++ ")"

    fwdDecl ty = let n = typeName ty
                 in case ty of
                      SP.BuiltIn t _ _ -> case builtInToNative $ unTBuiltIn t of
                                            Just native -> "typedef " ++ native ++ " " ++ n ++ ";"
                                            Nothing -> "/* No native renaming for type " ++ n ++ ".*/"
                      SP.Scalar t _ _ -> "typedef " ++ (show . scalarRepr $ t) ++ " " ++ n ++ "_t;"
                      SP.Const t _ _ -> "typedef " ++ (show . constRepr $ t) ++ " " ++ n ++ "_t;"
                      SP.FixedArray {} -> "struct " ++ n ++ ";"
                      SP.BoundedArray {} -> "struct " ++ n ++ ";"
                      SP.Struct {} -> "struct " ++ n ++ ";"
                      SP.Set {} -> "struct " ++ n ++ ";"
                      SP.Enum {} -> "struct " ++ n ++ ";"
                      SP.Partial {} -> "struct " ++ n ++ ";"
                      SP.Pad {} -> "struct " ++ n ++ ";"

    sTypes = specTypes spec
    libName = specNameToCName $ SP.specName spec
    libVersion = specVerToCVer $ SP.specVersion spec
    libInclude f = text $ "#include <" ++ f ++ ">"
    relInclude f = text $ "#include \"" ++ f ++ "\""
    hashBytes = bytesToCArray $ hashToBytes $ SP.specHash spec



blankLine :: Doc
blankLine = text ""

specNameToCName :: Name -> Name
specNameToCName n = "lib" ++ replaceWith n ' ' '_'

specVerToCVer :: Version -> String
specVerToCVer = id

replaceWith :: (Eq a) => [a] -> a -> a -> [a]
replaceWith xs srch repl = map replFn xs
  where
    replFn x = if x == srch then repl else x

bytesToCArray :: [Word8] -> String
bytesToCArray bs = "{" ++ strBytes ++ "}"
  where
    strBytes = intercalate ",\n   " $ map (intercalate ",") $ chunksOf 4 $ map showByte bs
    showByte b = let s = showHex b ""
                 in case s of
                      [b1,b2] -> "0x" ++ [toUpper b1,toUpper b2]
                      [b1] -> "0x0" ++ [toUpper b1]
                      _ -> error "This should be impossible."

builtInToNative :: BuiltIn -> Maybe String
builtInToNative BIu8       = Just "uint8_t"
builtInToNative BIu16      = Just "uint16_t"
builtInToNative BIu32      = Just "uint32_t"
builtInToNative BIu64      = Just "uint64_t"
builtInToNative BIs8       = Just "int8_t;"
builtInToNative BIs16      = Just "int16_t"
builtInToNative BIs32      = Just "int32_t"
builtInToNative BIs64      = Just "int64_t"
builtInToNative BIieee754s = Just "float"
builtInToNative BIieee754d = Just "double"
builtInToNative BIbool     = Nothing -- The builtin Boolean relies on 'bool' from stdbool.h
builtInToNative BIvoid     = Nothing -- The builtin Void type is unexpressable in C

{-
typeInfoBody :: M.Map Name TypeInfo -> TypeInfo -> IO TypeInfo
typeInfoBody m i = do
  b <- case tyInfType i of
          SP.BuiltIn {} -> typeBody' BuiltInInfo { biName = tyName }
          SP.Scalar {} -> typeBody' ScalarInfo { scName = tyName }
          SP.Const {} -> typeBody' ConstInfo { cName = tyName }
          SP.FixedArray a _ _ -> 
            let r = fixedArrRef a
                ri = fromJust $ M.lookup r m -- This *should* never fail.
            in typeBody' FixedArrayInfo { faName = tyName
                                        , faLength = (fixedArrLen . SP.unFixed) $ tyInfType i
                                        , faRefTypeDecl = tyInfDecl ri
                                        }
          SP.BoundedArray a _ _ alr -> 
            let r = boundedArrRef a
                ri = fromJust $ M.lookup r m -- This *should* never fail.
            in typeBody' BoundedArrayInfo { baName = tyName
                                          , baLength = (boundedArrMaxLen . SP.unBounded) $ tyInfType i
                                          , baRefTypeDecl = tyInfDecl ri
                                          , baLengthRepr = show $ unLengthRepr alr
                                          }
          SP.Struct s _ _ ->
            let sfs = map fromField $ unFields . structFields $ s
            in typeBody' StructInfo { sName = tyName
                                    , sFields = sfs
                                    }
          SP.Set s _ _ fr ->
            let sfs = map fromField $ unFields . setFields $ s
            in typeBody' SetInfo { tName = tyName
                                 , tFields = sfs
                                 , tFlagRepr = show . unFlagsRepr $ fr
                                 }
          SP.Enum e _ _ tr ->
            let sfs = map fromField $ unFields . enumFields $ e
            in typeBody' EnumInfo { eName = tyName
                                  , eFields = sfs
                                  , eTagRepr = show . unTagRepr $ tr
                                  }
          SP.Partial e _ _ tr lr ->
            let sfs = map fromField $ unFields . partialFields $ e
            in typeBody' PartialInfo { pName = tyName
                                     , pFields = sfs
                                     , pTagRepr = show . unTagRepr $ tr
                                     , pLengthRepr = show . unLengthRepr $ lr
                                     }
          SP.Pad {} -> typeBody' PadInfo { paName = tyName }
  return i { tyInfDeclBody = b }
  where
    tyName = tyInfName i
    typeBody' ctx = liftM T.unpack $ typeBody (tyInfBodyTemplName i) ctx
    fromField (IndexedRef n r ix) = let r' = fromJust $ M.lookup r m -- This *should* never fail.
                                        r'' = tyInfDecl r'
                                    in if "void" == r''
                                          then FieldInfo n "" ix
                                          else FieldInfo n r'' ix


typeInfo :: SP.SpType Name -> TypeInfo
typeInfo o = o' { tyInfBodyTemplName = typeBodyTempl o
                , tyInfSerTempName = typeSerTempl o
                , tyInfType = o
                }
  where
    o' = case o of
      SP.BuiltIn t _ _ ->
        baseInf
          { tyInfFwdDecls = [ builtInToTyDef $ unTBuiltIn t ]
          , tyInfDecl = case tyName of
                          "void" -> ""
                          n -> n
          }
      SP.Scalar t _ _ ->
        baseInf
          { tyInfFwdDecls = [ "typedef " ++ (show . scalarRepr $ t) ++ " " ++ tyName ++ "_t;"]
          , tyInfDecl = tyName ++ "_t"
          }
      SP.Const t _ _ ->
        baseInf
          { tyInfConsts = [ ConstInf "VALUE" (show . constValue . unConst $ o)]
          , tyInfFwdDecls = [ "typedef " ++ (show . constRepr $ t) ++ " " ++ tyName ++ "_t;"]
          , tyInfDecl = tyName ++ "_t"
          }
      SP.FixedArray t _ _ ->
        let arrLenStr = show . fixedArrLen $ t
        in baseInf
           { tyInfConsts = [ ConstInf "LENGTH" arrLenStr]
           , tyInfFwdDecls = [ "struct " ++ tyName ++ ";"]
           , tyInfDecl = "struct " ++ tyName
           }
      SP.BoundedArray t _ _ _ ->
        let arrMaxLenStr = show . boundedArrMaxLen $ t
        in baseInf
          { tyInfConsts = [ ConstInf "MAX_LENGTH" arrMaxLenStr]
          , tyInfFwdDecls = [ "struct " ++ tyName ++ ";"]
          , tyInfDecl = "struct " ++ tyName
          }
      SP.Struct {} ->
        baseInf
          { tyInfFwdDecls = [ "struct " ++ tyName ++ ";"]
          , tyInfDecl = "struct " ++ tyName
          }
      SP.Set {} ->
        baseInf
          { tyInfFwdDecls = [ "struct " ++ tyName ++ ";"]
          , tyInfDecl = "struct " ++ tyName
          }
      SP.Enum {} ->
        baseInf
          { tyInfFwdDecls = [ "struct " ++ tyName ++ ";"]
          , tyInfDecl = "struct " ++ tyName
          }
      SP.Partial {} ->
        baseInf
          { tyInfFwdDecls = [ "struct " ++ tyName ++ ";"]
          , tyInfDecl = "struct " ++ tyName
          }
      SP.Pad t _ _ ->
        baseInf
          { tyInfFwdDecls = [ "typedef u8 " ++ tyName ++ "_t[" ++ (show . padLength $ t) ++ "]" ]
          , tyInfDecl = tyName ++ "_t"
          }
    tyName = SP.typeName o
    baseInf = defaultTypeInfo { tyInfName = tyName
                              , tyInfHash = bytesToCArray $ hashToBytes $ SP.spHash o
                              , tyInfSize = RangeSize (minSize o) (maxSize o)
                              }
-}
