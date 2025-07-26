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
    -- is contained within the generated code.
    assertBool ("Generated code does not contain the expected implementation for to_convex_value:\n\nExpected to find:\n" ++ expected ++ "\n\nIn generated code:\n" ++ generatedCode) (normalizeString expected `isInfixOf` normalizeString generatedCode)

-- | Removes all whitespace/newlines etc. from the string.
normalizeString :: String -> String
normalizeString = filter (/= '\n') . filter (/= ' ') . filter (/= '\t')

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
    [ "impl DocumentsCreateComplexDocArgPayloadObject {",
      "    pub fn to_convex_value(&self) -> Result<Value, ApiError> {",
      "        let mut btmap = BTreeMap::new();",
      "            if let Some(v) = &self.name {",
      "    btmap.insert(\"name\".to_string(), Value::from(v.to_string()));",
      "}",
      "            btmap.insert(\"value\".to_string(), Value::from(self.value));",
      "            if let Some(v) = &self.tags {",
      "    btmap.insert(\"tags\".to_string(), Value::Array(v.iter().map(|item| item.to_convex_value()?).collect::<Result<Vec<_>, _>>()?));",
      "}",
      "        Ok(Value::Object(btmap))",
      "    }",
      "}"
    ]

getAssetsFunction :: Action.ConvexFunction
getAssetsFunction =
  Action.ConvexFunction
    { Action.funcName = "getAssets",
      Action.funcPath = "functions/assets",
      Action.funcType = Action.Query,
      Action.funcArgs = [("projectId", Schema.VId "projects")],
      Action.funcReturn =
        Schema.VArray
          ( Schema.VObject
              [ ("_id", Schema.VId "assets"),
                ("_creationTime", Schema.VNumber),
                ("project_id", Schema.VId "projects"),
                ("asset_name", Schema.VString),
                ( "link_metadata",
                  Schema.VObject
                    [ ("summary", Schema.VBytes),
                      ("sample_rate", Schema.VInt64),
                      ("length", Schema.VInt64)
                    ]
                ),
                ("asset_essence_mtime", Schema.VInt64)
              ]
          )
    }

expectedGetAssetsSerializationImpl :: String
expectedGetAssetsSerializationImpl =
  unlines
    [ "impl TryFrom<Value> for GetAssetsReturnObject {",
      "    type Error = ApiError;",
      "    fn try_from(value: Value) -> Result<Self, Self::Error> {",
      "        let obj = match value {",
      "            Value::Object(map) => map,",
      "            _ => return Err(ApiError::ConvexClientError(\"Expected object\".to_string())),",
      "        };",
      "        fn get__id(map: &BTreeMap<String, Value>, key: &str) -> Result<Id<types::AssetsDoc>, ApiError> {",
      "    match map.get(key) {",
      "        Some(v) => {",
      "            Ok(FromConvexValue::from_convex(v.clone())?)",
      "        }",
      "        _ => return Err(ApiError::ConvexClientError(format!(\"Expected field (Id<types::AssetsDoc>) '{}' not found\", key))),",
      "    }",
      "}",
      "        fn get__creation_time(map: &BTreeMap<String, Value>, key: &str) -> Result<f64, ApiError> {",
      "    match map.get(key) {",
      "        Some(v) => {",
      "            Ok(FromConvexValue::from_convex(v.clone())?)",
      "        }",
      "        _ => return Err(ApiError::ConvexClientError(format!(\"Expected field (f64) '{}' not found\", key))),",
      "    }",
      "}",
      "        fn get_project_id(map: &BTreeMap<String, Value>, key: &str) -> Result<Id<types::ProjectsDoc>, ApiError> {",
      "    match map.get(key) {",
      "        Some(v) => {",
      "            Ok(FromConvexValue::from_convex(v.clone())?)",
      "        }",
      "        _ => return Err(ApiError::ConvexClientError(format!(\"Expected field (Id<types::ProjectsDoc>) '{}' not found\", key))),",
      "    }",
      "}",
      "        fn get_asset_name(map: &BTreeMap<String, Value>, key: &str) -> Result<String, ApiError> {",
      "    match map.get(key) {",
      "        Some(v) => {",
      "            Ok(FromConvexValue::from_convex(v.clone())?)",
      "        }",
      "        _ => return Err(ApiError::ConvexClientError(format!(\"Expected field (String) '{}' not found\", key))),",
      "    }",
      "}",
      "        fn get_link_metadata(map: &BTreeMap<String, Value>, key: &str) -> Result<types::GetAssetsReturnLinkMetadataObject, ApiError> {",
      "    match map.get(key) {",
      "        Some(v) => {",
      "            Ok(FromConvexValue::from_convex(v.clone())?)",
      "        }",
      "        _ => return Err(ApiError::ConvexClientError(format!(\"Expected field (types::GetAssetsReturnLinkMetadataObject) '{}' not found\", key))),",
      "    }",
      "}",
      "        fn get_asset_essence_mtime(map: &BTreeMap<String, Value>, key: &str) -> Result<i64, ApiError> {",
      "    match map.get(key) {",
      "        Some(v) => {",
      "            Ok(FromConvexValue::from_convex(v.clone())?)",
      "        }",
      "        _ => return Err(ApiError::ConvexClientError(format!(\"Expected field (i64) '{}' not found\", key))),",
      "    }",
      "}",
      "                Ok(GetAssetsReturnObject {",
      "                                _id: get__id(&obj, \"_id\")?,",
      "            _creation_time: get__creation_time(&obj, \"_creationTime\")?,",
      "            project_id: get_project_id(&obj, \"project_id\")?,",
      "            asset_name: get_asset_name(&obj, \"asset_name\")?,",
      "            link_metadata: get_link_metadata(&obj, \"link_metadata\")?,",
      "            asset_essence_mtime: get_asset_essence_mtime(&obj, \"asset_essence_mtime\")?",
      "                })",
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
          expectedComplexSerializationImpl,
        runSerializationTest
          "generates correct from_convex for getAssets function"
          getAssetsFunction
          expectedGetAssetsSerializationImpl
      ]

-- Helper to check for substring containment.
isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
