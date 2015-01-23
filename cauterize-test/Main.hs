module Main (main) where

import Cauterize.Schema.Arbitrary
import qualified Cauterize.Schema as SC
import qualified Cauterize.Specification as SP

import Options.Applicative
import Test.QuickCheck.Gen
import Text.PrettyPrint.Class
import qualified Data.Set as S

data CautTestOpts = CautTestOpts String String
  deriving (Show)

{- 
 - TODO:
 -    - allowed prototypes
 -    - maximum length
 -    - minimum length
 -}

optParser :: Parser CautTestOpts
optParser = CautTestOpts
  <$> strOption ( long "output" <> metavar "OUTPUT" <> help outputHelp)
  <*> strOption ( long "count"  <> metavar "COUNT"  <> help countHelp)
  where
    outputHelp = "Output type. Either 'schema' or 'specification'."
    countHelp = "The number of types to generate."

options :: ParserInfo CautTestOpts
options = info (optParser <**> helper)
               (fullDesc <> progDesc "Process Cauterize schema files.")

runWithOptions :: (CautTestOpts -> IO ()) -> IO ()
runWithOptions fn = execParser options >>= fn

main :: IO ()
main = runWithOptions printArbSpec

printArbSpec :: CautTestOpts -> IO ()
printArbSpec (CautTestOpts out count) = do
  case out of
    "schema" -> outputCaut id allProtoParams count'
    "specification" -> outputCaut SP.fromSchema allProtoParams count'
    _ -> error $ "I don't know anything about '" ++ show out ++ "'."
  where
    count' = case reads count of
               [(v,"")] -> v
               _ -> error $ "Invalid count: " ++ count

outputCaut :: Pretty s => (SC.Schema -> s) -> S.Set ProtoParam -> Int -> IO ()
outputCaut fn ps c = do
  s <- mkASchema ps c
  case s of
    Left es -> print es
    Right s' -> print . pretty $ fn s'

{- TODO: When dealing with arrays, it's likely that the maximum depth we should
 - use is 5. Possibly 4. The size of arrays is exponential and this tends to
 - make huge huge huge huge huge types. -}
mkASchema :: S.Set ProtoParam -> Int -> IO (Either String SC.Schema)
mkASchema ps c = do
  s <- generate $ arbSchemaParam ps c
  case SC.checkSchema s of
    [] -> return (Right s)
    es -> return $ Left (show es)
