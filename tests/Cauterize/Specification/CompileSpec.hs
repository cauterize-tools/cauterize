{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Specification.CompileSpec
       ( spec
       ) where

import Cauterize.Specification.Types
import Cauterize.Schema.Parser
import Cauterize.Specification.Compile
import Cauterize.CommonTypes
import Test.Hspec

import qualified Data.Text as T
import qualified Data.List as L


comp :: T.Text -> Either String Specification
comp p = either (\e -> Left e)
                (\s -> Right (mkSpecification s))
                (parseSchema p)

spec :: Spec
spec = do
  describe "compile" $ do
    it "produces a synonym" $ do
      let c = comp synFoo
      c `shouldSatisfy` typeNameIs "foo"
      c `shouldSatisfy` typeIsSynonym
    it "produces an enumeration" $ do
      let c = comp enumBar
      c `shouldSatisfy` typeNameIs "bar"
      c `shouldSatisfy` typeIsEnumeration
      enumIndicies c `shouldBe` [1..4]
    it "produces a union" $ do
      let c = comp unionBaz
      c `shouldSatisfy` typeNameIs "baz"
      c `shouldSatisfy` typeIsUnion
      unionIndicies c `shouldBe` [1..2]
    it "produces a record" $ do
      let c = comp recordFizz
      c `shouldSatisfy` typeNameIs "fizz"
      c `shouldSatisfy` typeIsRecord
      recordIndicies c `shouldBe` [1..3]

    it "picks the right tag type (T1)" $ do
      let c = comp (enumLong 254)
      c `shouldSatisfy` typeNameIs "long"
      c `shouldSatisfy` typeIsEnumeration
      enumIndicies c `shouldBe` [1..254]
      enumTag c `shouldBe` Just T1

    it "picks the right tag type (T2)" $ do
      let c = comp (enumLong 255)
      c `shouldSatisfy` typeNameIs "long"
      c `shouldSatisfy` typeIsEnumeration
      enumIndicies c `shouldBe` [1..255]
      enumTag c `shouldBe` Just T2

synFoo :: T.Text
synFoo = "(type foo synonym u8)"

enumBar :: T.Text
enumBar = "(type bar enumeration (values a b c d))"

enumLong :: Int -> T.Text
enumLong 0 = error "Must not be 0."
enumLong c = let prefix = "(type long enumeration (values "
                 vals = map (\v -> "v_" ++ show v) (take c [0..] :: [Int])
                 valstr = concat $ L.intersperse " " vals
                 postfix = "))"
             in prefix `T.append` T.pack valstr `T.append` postfix

unionBaz :: T.Text
unionBaz = "(type baz union (fields (field a u32) (field b u16)))"

recordFizz :: T.Text
recordFizz = "(type fizz record (fields (field a u32) (field b u16) (field c f32)))"


typeNameIs :: Identifier -> Either a Specification -> Bool
typeNameIs n (Right (Specification { specTypes = [ Type { typeName = name } ] })) = n == name
typeNameIs _ _ = False

typeIsSynonym :: Either a Specification -> Bool
typeIsSynonym (Right
               (Specification { specTypes = [ Type { typeDesc = Synonym {} } ] })) = True
typeIsSynonym _ = False

typeIsEnumeration :: Either a Specification -> Bool
typeIsEnumeration (Right
                   (Specification { specTypes = [ Type { typeDesc = Enumeration {} } ] })) = True
typeIsEnumeration _ = False

typeIsUnion :: Either a Specification -> Bool
typeIsUnion (Right
             (Specification { specTypes = [ Type { typeDesc = Union {} } ] })) = True
typeIsUnion _ = False

typeIsRecord :: Either a Specification -> Bool
typeIsRecord (Right
              (Specification { specTypes = [ Type { typeDesc = Record {} } ] })) = True
typeIsRecord _ = False

enumIndicies :: Either a Specification -> [Integer]
enumIndicies (Right (Specification { specTypes = [ Type { typeDesc = desc } ] } ) ) =
  case desc of
    Enumeration { enumerationValues = vals } -> map getIndex vals
    _ -> []
  where
    getIndex (EnumVal { enumValIndex = i }) = i
enumIndicies _ = []

unionIndicies :: Either a Specification -> [Integer]
unionIndicies (Right (Specification { specTypes = [ Type { typeDesc = desc } ] } ) ) =
  case desc of
    Union { unionFields = fs } -> map fieldIndex fs
    _ -> []
unionIndicies _ = []

recordIndicies :: Either a Specification -> [Integer]
recordIndicies (Right (Specification { specTypes = [ Type { typeDesc = desc } ] } ) ) =
  case desc of
    Record { recordFields = fs } -> map fieldIndex fs
    _ -> []
recordIndicies _ = []


enumTag :: Either a Specification -> Maybe Tag
enumTag (Right (Specification { specTypes = [ Type { typeDesc = desc } ] } ) ) =
  case desc of
    Enumeration { enumerationTag = t } -> Just t
    _ -> Nothing
enumTag _ = Nothing
