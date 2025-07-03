module Main (main) where

import qualified ActionParserTest
import qualified ApiParserTest
import qualified RustSerializationTest
import qualified SchemaParserTest
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

main :: IO ()
main = do
  -- Combine all test suites into a single list
  let allTests =
        TestList
          [ ApiParserTest.tests,
            ActionParserTest.tests,
            SchemaParserTest.tests,
            RustSerializationTest.tests
          ]

  -- Run the combined test suite
  counts <- runTestTT allTests
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure
