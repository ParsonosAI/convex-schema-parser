{-# LANGUAGE OverloadedStrings #-}

module RustSerializationTest (tests) where

import qualified Backend.Rust as Rust
import qualified Convex.Action.Parser as Action
import qualified Convex.Parser as Convex
import qualified Convex.Schema.Parser as Schema
import Data.List (isPrefixOf, tails)
import qualified Data.Map as Map
import Test.HUnit
import Text.Parsec (runParserT)

-- Helper to run the test
runSerializationTest :: String -> Action.ConvexFunction -> String -> Test
runSerializationTest testName func expected =
  testName ~: TestCase $ do
    -- We create a dummy project containing only our test function.
    let project =
          Convex.ParsedProject
            { Convex.ppConstants = Map.empty,
              Convex.ppSchema = Schema.Schema {Schema.getTables = []},
              Convex.ppFunctions = [func]
            }
    -- Generate the Rust code.
    let generatedCode = Rust.generateRustCode project
    -- For simplicity in this test, we are just checking if the expected output
    -- is contained within the generated code. A more robust test could parse
    -- the Rust code, but this is sufficient to validate our specific goal.
    assertBool ("Generated code does not contain the expected implementation for to_convex_value:\n\nExpected to find:\n" ++ expected ++ "\n\nIn generated code:\n" ++ generatedCode) (expected `isInfixOf` generatedCode)

-- Test Input: A complex nested object definition
complexCreateFunction :: Action.ConvexFunction
complexCreateFunction =
  Action.ConvexFunction
    { Action.funcName = "createComplexDoc",
      Action.funcPath = "documents",
      Action.funcType = Action.Mutation,
      Action.funcArgs =
        [ ( "payload",
            Schema.VObject
              [ ("name", Schema.VOptional Schema.VString),
                ("value", Schema.VInt64),
                ( "tags",
                  Schema.VOptional
                    ( Schema.VArray
                        ( Schema.VObject
                            [ ("tag_name", Schema.VString),
                              ("tag_type", Schema.VReference "TagTypeEnum")
                            ]
                        )
                    )
                )
              ]
          )
        ],
      Action.funcReturn = Schema.VId "documents"
    }

-- The exact, correct Rust code we expect the generator to produce.
expectedComplexSerializationImpl :: String
expectedComplexSerializationImpl =
  unlines
    [ "impl types::CreateComplexDocPayloadObject {",
      "    pub fn to_convex_value(&self) -> Result<Value, ApiError> {",
      "        let mut map = BTreeMap::new();",
      "        if let Some(v) = &self.name {",
      "            map.insert(\"name\".to_string(), Value::from(v.clone()));",
      "        }",
      "        map.insert(\"value\".to_string(), Value::from(self.value));",
      "        if let Some(v) = &self.tags {",
      "            let vec_of_values: Result<Vec<Value>, ApiError> = v.iter().map(|item| item.to_convex_value()).collect();",
      "            map.insert(\"tags\".to_string(), Value::Array(vec_of_values?));",
      "        }",
      "        Ok(Value::Object(map))",
      "    }",
      "}"
    ]

-- Main test suite
tests :: Test
tests =
  "Rust Serialization"
    ~: test
      [ runSerializationTest
          "generates correct to_convex_value for complex nested objects"
          complexCreateFunction
          expectedComplexSerializationImpl
      ]

-- Helper to check for substring containment.
isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
