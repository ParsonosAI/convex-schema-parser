{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiParserTest (tests) where

-- Import the module and function we are testing
import qualified Convex.Parser as APIParser
import Test.HUnit
import Text.Parsec (parse)

-- The test suite for the apiFileParser, exported to be used by Main.
tests :: Test
tests =
  "API File Parser"
    ~: runTest
      "parses module paths from api.d.ts"
      (APIParser.apiFileParser)
      sampleApiFile
      expectedApiPaths
  where
    runTest testName parser input expected =
      testName ~: TestCase $ do
        -- Use the pure `parse` function since this parser is stateless.
        case parse parser "(test input)" input of
          Left err -> assertFailure ("Parser failed: " ++ show err)
          Right actual -> actual @?= expected

-- A sample input string that mimics the structure of a real api.d.ts file.
sampleApiFile :: String
sampleApiFile =
  unlines
    [ "/* eslint-disable */",
      "import type * as admin_actions from \"../admin/actions.js\";",
      "import type * as custom from \"../custom.js\";",
      "declare const fullApi: ApiFromModules<{",
      "  \"admin/actions\": typeof admin_actions;",
      "  custom: typeof custom;",
      "  \"stripe/checkout\": typeof stripe_checkout;",
      "}>;"
    ]

-- The list of module paths we expect the parser to extract.
expectedApiPaths :: [String]
expectedApiPaths =
  [ "admin/actions",
    "custom",
    "stripe/checkout"
  ]
