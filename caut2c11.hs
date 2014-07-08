{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Main where

import qualified Data.Text.Lazy.IO as TL
import Text.Hastache
import Text.Hastache.Context
import Data.Data 
import qualified Data.Text.Lazy as T
import Data.Word 
import Data.List 
import Data.Char 
import Data.Maybe 
import qualified Data.Traversable as TR
import qualified Data.Map as M
import Control.Monad
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Options.Applicative
import Numeric

import Paths_cauterize

import Cauterize.FormHash
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
      s'' <- infoFromSpec s'
      let context = mkGenericContext s''
      
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

infoFromSpec :: SP.Spec Name -> IO TemplInfo
infoFromSpec sp = do
  tts <- mapM (typeInfoBody m) mTypes
  return
    TemplInfo { templName = specNameToCName $ SP.specName sp
              , templVersion = specVerToCVer $ SP.specVersion sp
              , templHash = bytesToCArray hashBytes
              , templHashSize = length hashBytes
              , templSize = SP.specSize sp
              , templTypes = tts
              }
  where
    hashBytes = hashToBytes $ SP.specHash sp
    specTyMap = M.fromList $ map (\t -> (SP.typeName t, t)) $ SP.specTypes sp
    m = fmap typeInfo specTyMap
    mTypes = map snd $ M.toList m

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

typeBody :: Data a => FilePath -> a -> IO T.Text
typeBody fname ctx = do
  fpath <- getDataFileName fname
  hastacheFile muConfig fpath ctx'
  where
    ctx' = mkGenericContext ctx

typeBodyTempl :: SP.SpType Name -> FilePath
typeBodyTempl i =
  case i of
    SP.BuiltIn {} -> "types/builtin_body.tmpl.c" 
    SP.Scalar {} -> "types/scalar_body.tmpl.c" 
    SP.Const {} -> "types/const_body.tmpl.c" 
    SP.FixedArray {} -> "types/fixed_body.tmpl.c" 
    SP.BoundedArray {} -> "types/bounded_body.tmpl.c" 
    SP.Struct {} -> "types/struct_body.tmpl.c" 
    SP.Set {} -> "types/set_body.tmpl.c" 
    SP.Enum {} -> "types/enum_body.tmpl.c" 
    SP.Partial {} -> "types/partial_body.tmpl.c" 
    SP.Pad {} -> "types/pad_body.tmpl.c" 

typeInfoBody :: M.Map Name TypeInfo -> TypeInfo -> IO TypeInfo
typeInfoBody m i = do
  b <- case tyInfType i of
          SP.FixedArray {} -> 
            let a = SP.unFixed $ tyInfType i
                r = fixedArrRef a
                ri = fromJust $ M.lookup r m -- This *should* never fail.
            in typeBody' FixedArrayInfo { faName = tyInfName i
                                        , faLength = (fixedArrLen . SP.unFixed) $ tyInfType i
                                        , faRefTypeDecl = tyInfDecl ri
                                        }
          SP.BoundedArray {} -> 
            let t = tyInfType i
                a = SP.unBounded t
                r = boundedArrRef a
                ri = fromJust $ M.lookup r m -- This *should* never fail.
                alr = lenRepr t
            in typeBody' BoundedArrayInfo { baName = tyInfName i
                                          , baLength = (boundedArrMaxLen . SP.unBounded) $ tyInfType i
                                          , baRefTypeDecl = tyInfDecl ri
                                          , baLengthRepr = show $ unLengthRepr alr
                                          }
          _ -> return "!!! A DECL BODY !!!"
  return i { tyInfDeclBody = b }
  where
    typeBody' ctx = liftM T.unpack $ typeBody (tyInfTemplName i) ctx

typeInfo :: SP.SpType Name -> TypeInfo
typeInfo o = o' { tyInfDeclBody = "!! UNIMPLEMENTED BODY !!"
                , tyInfTemplName = typeBodyTempl o
                , tyInfType = o
                }
  where
    -- typeBody' ctx = liftM T.unpack $ typeBody (typeBodyTempl o) ctx
    o' = case o of
      SP.BuiltIn t _ _ ->
        baseInf
          { tyInfFwdDecls = [ builtInToTyDef $ unTBuiltIn t ]
          , tyInfDecl = tyName
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
      SP.BoundedArray t _ _ lr ->
        let arrMaxLenStr = show . boundedArrMaxLen $ t
            arrMaxLenReprStr = show . unLengthRepr $ lr 
        in baseInf
          { tyInfConsts = [ ConstInf "MAX_LENGTH" arrMaxLenStr]
          , tyInfFwdDecls = [ "struct " ++ tyName ++ ";"]
          -- , tyInfDeclBody = "struct " ++ tyName ++ " { "
          --                        ++ arrMaxLenReprStr ++ " len; "
          --                        ++ boundedArrRef t ++ " elems[" ++ arrMaxLenStr ++ "]; };"
          , tyInfDecl = "struct " ++ tyName
          }
      SP.Struct {} ->
        baseInf
          { tyInfFwdDecls = [ "struct " ++ tyName ++ ";"]
          -- , tyInfDeclBody = "!! STRUCT DECL !!"
          , tyInfDecl = "struct " ++ tyName
          }
      SP.Set {} ->
        baseInf
          { tyInfFwdDecls = [ "struct " ++ tyName ++ ";"]
          -- , tyInfDeclBody = "!! SET DECL !!"
          , tyInfDecl = "struct " ++ tyName
          }
      SP.Enum {} ->
        baseInf
          { tyInfFwdDecls = [ "enum " ++ tyName ++ ";"]
          -- , tyInfDeclBody = "!! ENUM DECL !!"
          , tyInfDecl = "enum " ++ tyName
          }
      SP.Partial {} ->
        baseInf
          { tyInfFwdDecls = [ "struct " ++ tyName ++ ";"]
          -- , tyInfDeclBody = "!! SET DECL !!"
          , tyInfDecl = "struct " ++ tyName
          }
      SP.Pad {} ->
        baseInf
          { tyInfFwdDecls = [ "/* Padding for " ++ tyName ++ " can't ever be instantiated. */"]
          }
      _ ->
        baseInf
          { tyInfConsts = []
          , tyInfFwdDecls = ["SOME " ++ tyName ++ ";"]
          , tyInfDeclBody = []
          }
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

data TypeInfo = TypeInfo
  { tyInfName :: Name
  , tyInfHash :: String
  , tyInfSize :: RangeSize
  , tyInfConsts :: [ConstInf]
  , tyInfFwdDecls :: [String]
  , tyInfDeclBody :: String
  , tyInfDecl :: String
  , tyInfPacker :: String
  , tyInfUnpacker :: String
  , tyInfTemplName :: String
  , tyInfType :: SP.SpType Name
  } deriving (Data, Typeable)

data FixedArrayInfo = FixedArrayInfo
  { faName :: Name
  , faLength :: Integer
  , faRefTypeDecl :: String
  } deriving (Data, Typeable)

data BoundedArrayInfo = BoundedArrayInfo
  { baName :: Name
  , baLength :: Integer
  , baRefTypeDecl :: String
  , baLengthRepr :: String
  } deriving (Data, Typeable)

defaultTypeInfo :: TypeInfo
defaultTypeInfo = TypeInfo "!name!" "!hash!" (RangeSize 0 0) [] [] "" "" "" "" "" (error "No defined type.")

data ConstInf = ConstInf
  { constInfName :: String
  , constInfValue :: String
  } deriving (Data, Typeable)

muConfig :: MuConfig IO
muConfig = defaultConfig { muEscapeFunc = emptyEscape }
