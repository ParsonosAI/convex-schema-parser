{-# LANGUAGE OverloadedStrings #-}

module SchemaParserTest (tests) where

import qualified Convex.Schema.Parser as Schema
import qualified Data.Map as Map
import Test.HUnit
import Text.Parsec (runParserT)

runSchemaTest :: String -> String -> Schema.ParsedFile -> Test
runSchemaTest testName input expected =
  testName ~: TestCase $ do
    result <- Schema.parseSchema input
    case result of
      Left err -> assertFailure ("Parser failed: " ++ show err)
      Right parsedFile -> parsedFile @?= expected

-- Test Case 1: A standard schema with mixed statement endings.
sampleSchemaStandard :: String
sampleSchemaStandard =
  unlines
    [ "import { defineSchema, defineTable, v } from \"convex/server\";",
      "",
      "export const roleEnum = v.union(v.literal(\"admin\"), v.literal(\"user\"));",
      "",
      "export const productEnum = v.union(v.literal(\"palaba\"), v.literal(\"audiorake\"))",
      "",
      "export default defineSchema({",
      "  users: defineTable({",
      "    name: v.string(),",
      "    role: roleEnum",
      "  }),",
      "});"
    ]

expectedSchemaStandard :: Schema.ParsedFile
expectedSchemaStandard =
  Schema.ParsedFile
    { Schema.parsedConstants =
        Map.fromList
          [ ("roleEnum", Schema.VUnion [Schema.VLiteral "admin", Schema.VLiteral "user"]),
            ("productEnum", Schema.VUnion [Schema.VLiteral "palaba", Schema.VLiteral "audiorake"])
          ],
      Schema.parsedSchema =
        Schema.Schema
          { Schema.getTables =
              [ Schema.Table
                  { Schema.tableName = "users",
                    Schema.tableFields =
                      [ Schema.Field "name" Schema.VString,
                        Schema.Field "role" (Schema.VReference "roleEnum")
                      ],
                    Schema.tableIndexes = []
                  }
              ]
          }
    }

-- Test Case 2: Schema definition appears at the top of the file.
sampleSchemaAtTop :: String
sampleSchemaAtTop =
  unlines
    [ "import { defineSchema, defineTable, v } from \"convex/server\";",
      "",
      "export default defineSchema({",
      "  projects: defineTable({",
      "    name: v.string(),",
      "    product: productEnum, // Reference to a const defined later",
      "  }),",
      "});",
      "",
      "// This constant is defined after it is referenced.",
      "export const productEnum = v.union(v.literal(\"a\"), v.literal(\"b\"));"
    ]

expectedSchemaAtTop :: Schema.ParsedFile
expectedSchemaAtTop =
  Schema.ParsedFile
    { Schema.parsedConstants =
        Map.fromList
          [("productEnum", Schema.VUnion [Schema.VLiteral "a", Schema.VLiteral "b"])],
      Schema.parsedSchema =
        Schema.Schema
          { Schema.getTables =
              [ Schema.Table
                  { Schema.tableName = "projects",
                    Schema.tableFields =
                      [ Schema.Field "name" Schema.VString,
                        Schema.Field "product" (Schema.VReference "productEnum")
                      ],
                    Schema.tableIndexes = []
                  }
              ]
          }
    }

-- Test Case 3: A table uses a reference to a const object for its definition.
sampleSchemaWithObjectRef :: String
sampleSchemaWithObjectRef =
  unlines
    [ "import { v, defineSchema, defineTable } from \"convex/server\";",
      "",
      "// A top-level constant defining the fields for a table.",
      "const authCodeValidator = {",
      "  exchange_code: v.string(),",
      "  tenant_id: v.id(\"tenants\"),",
      "};",
      "",
      "// A type alias, which should be parsed and stored.",
      "export type MyId = typeof v.id(\"tenants\");",
      "",
      "export default defineSchema({",
      "  // This table uses the constant directly.",
      "  auth_code_sessions: defineTable(authCodeValidator),",
      "});"
    ]

expectedSchemaWithObjectRef :: Schema.ParsedFile
expectedSchemaWithObjectRef =
  Schema.ParsedFile
    { Schema.parsedConstants =
        Map.fromList
          [ ( "authCodeValidator",
              Schema.VObject
                [ ("exchange_code", Schema.VString),
                  ("tenant_id", Schema.VId "tenants")
                ]
            ),
            ("MyId", Schema.VId "tenants")
          ],
      Schema.parsedSchema =
        Schema.Schema
          { Schema.getTables =
              [ Schema.Table
                  { Schema.tableName = "auth_code_sessions",
                    -- The parser should expand the reference into the actual fields.
                    Schema.tableFields =
                      [ Schema.Field "exchange_code" Schema.VString,
                        Schema.Field "tenant_id" (Schema.VId "tenants")
                      ],
                    Schema.tableIndexes = []
                  }
              ]
          }
    }

sampleComplexNoSemicolons :: String
sampleComplexNoSemicolons =
  unlines
    [ "import { v, defineSchema, defineTable } from \"convex/server\"",
      "",
      "export const statusEnum = v.union(v.literal(\"pending\"), v.literal(\"complete\"))",
      "",
      "export default defineSchema({",
      "  tasks: defineTable({",
      "    title: v.string(),",
      "    status: statusEnum,",
      "  }).index(\"by_status\", [\"status\"])",
      "   .index(\"by_title\", [\"title\"]),",
      "",
      "  // This table uses the constant directly.",
      "  sessions: defineTable(sessionValidator)",
      "})",
      "",
      "// This constant is defined after it is referenced.",
      "const sessionValidator = {",
      "  userId: v.id(\"users\"),",
      "  token: v.string()",
      "}",
      "",
      "export type ComplexType = typeof sessionValidator"
    ]

expectedComplexNoSemicolons :: Schema.ParsedFile
expectedComplexNoSemicolons =
  Schema.ParsedFile
    { Schema.parsedConstants =
        Map.fromList
          [ ("statusEnum", Schema.VUnion [Schema.VLiteral "pending", Schema.VLiteral "complete"]),
            ( "sessionValidator",
              Schema.VObject
                [ ("userId", Schema.VId "users"),
                  ("token", Schema.VString)
                ]
            ),
            ("ComplexType", Schema.VReference "sessionValidator")
          ],
      Schema.parsedSchema =
        Schema.Schema
          { Schema.getTables =
              [ Schema.Table
                  { Schema.tableName = "tasks",
                    Schema.tableFields =
                      [ Schema.Field "title" Schema.VString,
                        Schema.Field "status" (Schema.VReference "statusEnum")
                      ],
                    Schema.tableIndexes = [Schema.Index "by_status" ["status"], Schema.Index "by_title" ["title"]]
                  },
                Schema.Table
                  { Schema.tableName = "sessions",
                    Schema.tableFields =
                      [ Schema.Field "userId" (Schema.VId "users"),
                        Schema.Field "token" Schema.VString
                      ],
                    Schema.tableIndexes = []
                  }
              ]
          }
    }

sampleOptionalArrayOfObjects :: String
sampleOptionalArrayOfObjects =
  unlines
    [ "import { v, defineSchema, defineTable } from \"convex/server\"",
      "export const accessLevelEnum = v.union(v.literal(\"read\"), v.literal(\"write\"))",
      "export default defineSchema({",
      "  projects: defineTable({",
      "    shared_users: v.optional(",
      "      v.array(",
      "        v.object({",
      "          user_id: v.id(\"users\"),",
      "          access: accessLevelEnum",
      "        })",
      "      )",
      "    )",
      "  })",
      "})"
    ]

expectedOptionalArrayOfObjects :: Schema.ParsedFile
expectedOptionalArrayOfObjects =
  Schema.ParsedFile
    { Schema.parsedConstants =
        Map.fromList
          [("accessLevelEnum", Schema.VUnion [Schema.VLiteral "read", Schema.VLiteral "write"])],
      Schema.parsedSchema =
        Schema.Schema
          { Schema.getTables =
              [ Schema.Table
                  { Schema.tableName = "projects",
                    Schema.tableFields =
                      [ Schema.Field
                          "shared_users"
                          ( Schema.VOptional
                              ( Schema.VArray
                                  ( Schema.VObject
                                      [ ("user_id", Schema.VId "users"),
                                        ("access", Schema.VReference "accessLevelEnum")
                                      ]
                                  )
                              )
                          )
                      ],
                    Schema.tableIndexes = []
                  }
              ]
          }
    }

sampleArrayOfPrimitives :: String
sampleArrayOfPrimitives =
  unlines
    [ "import { v, defineSchema, defineTable } from \"convex/server\"",
      "export default defineSchema({",
      "  analytics: defineTable({",
      "    summary: v.array(v.number()),",
      "    length: v.number(),",
      "  })",
      "})"
    ]

expectedArrayOfPrimitives :: Schema.ParsedFile
expectedArrayOfPrimitives =
  Schema.ParsedFile
    { Schema.parsedConstants = Map.empty,
      Schema.parsedSchema =
        Schema.Schema
          { Schema.getTables =
              [ Schema.Table
                  { Schema.tableName = "analytics",
                    Schema.tableFields =
                      [ Schema.Field "summary" (Schema.VArray Schema.VNumber),
                        Schema.Field "length" Schema.VNumber
                      ],
                    Schema.tableIndexes = []
                  }
              ]
          }
    }

-- Main test suite combining all cases.
tests :: Test
tests =
  "Schema Parser"
    ~: test
      [ runSchemaTest "parses standard schema with mixed endings" sampleSchemaStandard expectedSchemaStandard,
        runSchemaTest "parses schema defined before its constants" sampleSchemaAtTop expectedSchemaAtTop,
        runSchemaTest "parses table defined with an object reference" sampleSchemaWithObjectRef expectedSchemaWithObjectRef,
        runSchemaTest "parses complex schema without semicolons" sampleComplexNoSemicolons expectedComplexNoSemicolons,
        runSchemaTest "parses optional array of objects" sampleOptionalArrayOfObjects expectedOptionalArrayOfObjects,
        runSchemaTest "parses array of primitives" sampleArrayOfPrimitives expectedArrayOfPrimitives
      ]
