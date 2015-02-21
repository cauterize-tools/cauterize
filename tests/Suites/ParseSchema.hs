module Suites.ParseSchema
  ( suite
  ) where

import TestSupport

suite :: [IO Int]
suite =
  [ testCase "can parse schema" testCanParseTestSchema
  ]

testCanParseTestSchema :: IO ()
testCanParseTestSchema = withTestS $ const (return ())
