module Main where

import Cauterize.Schema.Arbitrary

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Text.PrettyPrint
import Text.PrettyPrint.Class

main :: IO ()
main = do
  putStrLn "Test"
  -- a <- generate $ genTypeRuns 5
  -- putStrLn $ unlines $ map show a
  s <- generate arbSchema
  print $ pretty $ s
