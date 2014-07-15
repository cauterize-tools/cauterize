module Main where

import Options.Applicative
import Text.PrettyPrint as P hiding ((<>))

import Data.Word
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.List.Split
import Data.Maybe
import Numeric

import Cauterize.FormHash
import Cauterize.Specification as SP
import Cauterize.Common.Primitives
import Cauterize.Common.Types
import Cauterize.Common.IndexedRef

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
                                 , typeProtoTypes
                                 , blankLine
                                 , typeBodies
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
    typeProtoTypes = vcat $ concatMap funProtoTypes sTypes
    typeBodies = vcat $ map typeBody sTypes

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
                      SP.Scalar t _ _ -> "typedef " ++ (show . scalarRepr $ t) ++ " " ++ tyDecl ty ++ ";"
                      SP.Const t _ _ -> "typedef " ++ (show . constRepr $ t) ++ " " ++ tyDecl ty ++ ";"
                      SP.FixedArray {} -> tyDecl ty ++ ";"
                      SP.BoundedArray {} -> tyDecl ty ++ ";"
                      SP.Struct {} -> tyDecl ty ++ ";"
                      SP.Set {} -> tyDecl ty ++ ";"
                      SP.Enum {} -> tyDecl ty ++ ";"
                      SP.Partial {} -> tyDecl ty ++ ";"
                      SP.Pad {} -> tyDecl ty ++ ";"

    tyDecl ty = let n = typeName ty
                in case ty of
                     SP.BuiltIn {} -> if "void" == n
                                           then "ERROR: cannot declare type `void`."
                                           else n
                     SP.Scalar {} -> n
                     SP.Const {} -> n
                     SP.FixedArray {} -> "struct " ++ n
                     SP.BoundedArray {} -> "struct " ++ n
                     SP.Struct {} -> "struct " ++ n
                     SP.Set {} -> "struct " ++ n
                     SP.Enum {} -> "struct " ++ n
                     SP.Partial {} -> "struct " ++ n
                     SP.Pad {} -> "struct " ++ n

    funProtoTypes ty = let n = typeName ty
                           d = tyDecl ty
                           sizer = text $ "enum caut_status packed_size_" ++ n ++ "(" ++ d ++ " const * const obj);"
                           packer = text $ "enum caut_status pack_" ++ n ++ "(struct caut_pack_iter * const iter, " ++ d ++ " const * const obj);"
                           unpacker = text $ "enum caut_status unpack_" ++ n ++ "(struct caut_unpack_iter * const iter, " ++ d ++ " * const obj);"
                       in if n == "void"
                            then []
                            else [packer, unpacker, sizer, blankLine]

    typeBody ty = let n = typeName ty
                  in case ty of
                     SP.BuiltIn {} -> P.empty
                     SP.Scalar {} -> P.empty
                     SP.Const {} -> P.empty
                     SP.FixedArray t _ _ -> let refDecl = tyDeclFromName $ fixedArrRef t
                                            in text $ "struct " ++ n ++ " {\n" ++
                                                       "  " ++ refDecl ++ " elems[" ++ fullConstName n "LENGTH" ++ "];\n" ++
                                                       "};\n"
                     SP.BoundedArray t _ _ alr -> let alr' = show $ unLengthRepr alr
                                                      refDecl = tyDeclFromName $ boundedArrRef t
                                                  in text $ "struct " ++ n ++ " {\n" ++
                                                            "  " ++ alr' ++ " _length;\n\n" ++
                                                            "  " ++ refDecl ++ " elems[" ++ fullConstName n "MAX_LENGTH" ++ "];\n" ++
                                                            "};\n"
                     SP.Struct t _ _  -> text $ "struct " ++ n ++ " {\n" ++
                                                   concatMap (("  " ++) . fieldBody) (unFields . structFields $ t) ++
                                                "};\n"
                     SP.Set t _ _ fr -> text $ "struct " ++ n ++ " {\n" ++
                                                  "  " ++ tyDeclFromName (show . unFlagsRepr $ fr) ++ " _flags;\n\n" ++
                                                  concatMap (("  " ++) . fieldBody) (unFields . setFields $ t) ++
                                               "};\n"
                     SP.Enum t _ _ tr -> text $ "struct " ++ n ++ " {\n" ++
                                                "  enum " ++ n ++ "_tag {\n" ++
                                                   concatMap (\fn -> "    " ++ fieldName fn ++ " = " ++ (show . fieldIndex $ fn) ++ ",\n") (unFields . enumFields $ t) ++
                                                "  };\n\n" ++ 

                                                "  " ++ (tyDeclFromName . show . unTagRepr $ tr) ++ " _tag;\n\n" ++
                                                   concatMap (("  " ++) . fieldBody) (unFields . enumFields $ t) ++
                                                "};\n"
                     SP.Partial t _ _ tr lr -> text $ "struct " ++ n ++ " {\n" ++
                                                      "  enum " ++ n ++ "_tag {\n" ++
                                                         concatMap (\fn -> "    " ++ fieldName fn ++ " = " ++ (show . fieldIndex $ fn) ++ ",\n") (unFields . partialFields $ t) ++
                                                      "  };\n\n" ++ 

                                                      "  " ++ (tyDeclFromName . show . unTagRepr $ tr) ++ " _tag;\n" ++
                                                      "  " ++ (tyDeclFromName . show . unLengthRepr $ lr) ++ " _length;\n\n" ++
                                                         concatMap (("  " ++) . fieldBody) (unFields . partialFields $ t) ++
                                                      "};\n"
                     SP.Pad {} -> P.empty
      where
        fieldBody f = if fieldRef f == "void"
                        then "/* Field " ++ fieldName f ++ " is void. */\n"
                        else (tyDeclFromName . fieldRef $ f) ++ " " ++ fieldName f ++ ";\n"

    sTypes = specTypes spec
    mTypes = M.fromList $ zip (map typeName sTypes) sTypes
    tyDeclFromName n = let t = fromMaybe (error "Inconsistent map.") (M.lookup n mTypes)
                       in tyDecl t
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
