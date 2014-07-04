{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified Data.Text.Lazy.IO as TL
import Text.Hastache
import Text.Hastache.Context
import Data.Data 
import Data.Word 
import Data.List 
import Data.Char 
import Control.Monad
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Options.Applicative
import Numeric

import Paths_cauterize

import Cauterize.Schema as SC
import Cauterize.FormHash as SC
import Cauterize.Specification as SP
import Cauterize.Schema.Arbitrary
import Cauterize.Common.Primitives
import Cauterize.Common.Types

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
  hfile <- getDataFileName "libroot.tmpl.h"
  cfile <- getDataFileName "libroot.tmpl.c"

  s <- SP.parseFile (inputFile opts)

  case s of
    Left e -> print e
    Right s' -> do
      let context = mkGenericContext $ infoFromSpec s'
      
      putStrLn hfile
      hres <- hastacheFile muConfig hfile context
      TL.putStrLn hres

      putStrLn cfile
      cres <- hastacheFile muConfig cfile context
      TL.putStrLn cres

replaceWith :: (Eq a) => [a] -> a -> a -> [a]
replaceWith xs srch repl = map replFn xs
  where
    replFn x = if x == srch then repl else x

specNameToCName :: Name -> Name
specNameToCName n = "lib" ++ replaceWith n ' ' '_'

specVerToCVer :: Version -> String
specVerToCVer = id

bytesToCArray :: [Word8] -> String
bytesToCArray bs = "{" ++ strBytes ++ "}"
  where
    strBytes = intercalate "," $ map showByte bs
    showByte b = let s = showHex b ""
                 in case s of
                      [b1,b2] -> "0x" ++ [toUpper b1,toUpper b2]
                      [b1] -> "0x0" ++ [toUpper b1]
                      _ -> error "This should be impossible."

infoFromSpec :: SP.Spec Name -> TemplInfo
infoFromSpec sp =
  TemplInfo { templName = specNameToCName $ SP.specName sp
            , templVersion = specVerToCVer $ SP.specVersion sp
            , templHash = bytesToCArray hashBytes
            , templHashSize = length hashBytes
            , templSize = SP.specSize sp
            , templTypes = map typeInfo $ SP.specTypes sp
            }
  where
    hashBytes = hashToBytes $ SP.specHash sp

builtInToTyDef :: BuiltIn -> String
builtInToTyDef BIu8 = "typedef uint8_t u8;"
builtInToTyDef BIu16 = "typedef uint16_t u16;"
builtInToTyDef BIu32 = "typedef uint32_t u32;"
builtInToTyDef BIu64 = "typedef uint64_t u64;"
builtInToTyDef BIs8 = "typedef int8_t s8;"
builtInToTyDef BIs16 = "typedef int16_t s16;"
builtInToTyDef BIs32 = "typedef int32_t s32;"
builtInToTyDef BIs64 = "typedef int64_t s64;"
builtInToTyDef BIieee754s = "typedef float ieee754s;"
builtInToTyDef BIieee754d = "typedef double ieee754d;"
builtInToTyDef BIbool = "/* The builtin Boolean relies on 'bool' from stdbool.h */"
builtInToTyDef BIvoid = "/* The builtin Void type is unexpressable in C */"

typeInfo :: SP.SpType Name -> TypeInfo
typeInfo o =
  case o of
    SP.BuiltIn t _ _ ->
      baseInf
        { tyInfFwdDecls = [ builtInToTyDef $ unTBuiltIn t ]
        , tyInfDeclBodies = ["!! BUILTIN DECL !!"]
        , tyInfDecl = tyName
        }
    SP.Scalar t _ _ ->
      baseInf
        { tyInfFwdDecls = [ "typedef " ++ (show . scalarRepr $ t) ++ " " ++ tyName ++ "_t;"]
        , tyInfDeclBodies = ["!! SCALAR DECL !!"]
        , tyInfDecl = tyName ++ "_t"
        }
    SP.Const t _ _ ->
      baseInf
        { tyInfConsts = [ ConstInf "VALUE" (show . constValue . unConst $ o)]
        , tyInfFwdDecls = [ "typedef " ++ (show . constRepr $ t) ++ " " ++ tyName ++ "_t;"]
        , tyInfDeclBodies = ["!! SCALAR DECL !!"]
        , tyInfDecl = tyName ++ "_t"
        }
    SP.FixedArray t _ _ ->
      let arrLenStr = show . fixedArrLen $ t
      in baseInf
        { tyInfConsts = [ ConstInf "LENGTH" arrLenStr]
        , tyInfFwdDecls = [ "struct " ++ tyName ++ ";"]
        , tyInfDeclBodies = ["struct " ++ tyName ++ " { " ++ fixedArrRef t ++ " elems[" ++ arrLenStr ++ "]; };"]
        , tyInfDecl = "struct " ++ tyName
        }
    SP.BoundedArray t _ _ lr ->
      let arrMaxLenStr = show . boundedArrMaxLen $ t
          arrMaxLenReprStr = show . unLengthRepr $ lr 
      in baseInf
        { tyInfConsts = [ ConstInf "MAX_LENGTH" arrMaxLenStr]
        , tyInfFwdDecls = [ "struct " ++ tyName ++ ";"]
        , tyInfDeclBodies = ["struct " ++ tyName ++ " { " ++ arrMaxLenReprStr ++ " len; " ++ boundedArrRef t ++ " elems[" ++ arrMaxLenStr ++ "]; };"]
        , tyInfDecl = "struct " ++ tyName
        }
    _ ->
      baseInf
        { tyInfConsts = []
        , tyInfFwdDecls = ["SOME " ++ tyName ++ ";"]
        , tyInfDeclBodies = []
        }
  where
    tyName = SP.typeName o
    baseInf = defaultTypeInfo { tyInfName = tyName
                              , tyInfHash = bytesToCArray $ hashToBytes $ SP.spHash o
                              , tyInfSize = RangeSize (minSize o) (maxSize o)
                              }

data TemplInfo = TemplInfo
  { templName :: String
  , templVersion :: String
  , templHash :: String
  , templHashSize :: Int
  , templSize :: RangeSize
  , templTypes :: [TypeInfo]
  } deriving (Data, Typeable)

defaultTypeInfo :: TypeInfo
defaultTypeInfo = TypeInfo "!name!" "!hash!" (RangeSize 0 0) [] [] [] "!decl!"
data TypeInfo = TypeInfo
  { tyInfName :: Name
  , tyInfHash :: String
  , tyInfSize :: RangeSize
  , tyInfConsts :: [ConstInf]
  , tyInfFwdDecls :: [String]
  , tyInfDeclBodies :: [String]
  , tyInfDecl :: String
  } deriving (Data, Typeable)

data ConstInf = ConstInf
  { constInfName :: String
  , constInfValue :: String
  } deriving (Data, Typeable)

muConfig :: MuConfig IO
muConfig = defaultConfig { muEscapeFunc = emptyEscape }
