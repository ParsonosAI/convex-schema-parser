{-# LANGUAGE OverloadedStrings #-}

module UnificationTest (tests) where

import qualified Convex.Action.Parser as Action
import qualified Convex.Parser as Parser
import qualified Convex.Schema.Parser as Schema
import Test.HUnit

testUnifyWithTableDoc :: Test
testUnifyWithTableDoc = "unifies function return with table doc" ~: TestCase $ do
  let schemaContent =
        unlines
          [ "import { defineSchema, defineTable } from \"convex/server\";",
            "import { v } from \"convex/values\";",
            "",
            "export default defineSchema({",
            "  users: defineTable({",
            "    name: v.string(),",
            "    email: v.string()",
            "  })",
            "});"
          ]
  let apiFileContent =
        unlines
          [ "declare const fullApi: ApiFromModules<{",
            "  \"myActions\": typeof myActions;",
            "}>;"
          ]
  let actionContent =
        unlines
          [ "export declare const getUser: import(\"convex/server\").RegisteredQuery<\"public\", {}, Promise<{ _id: import(\"convex/values\").GenericId<\"users\">; _creationTime: number; name: string; email: string; }>>;"
          ]
  let actionContents = [("myActions", actionContent)]

  result <- Parser.parseProjectFromContents schemaContent apiFileContent actionContents
  case result of
    Left err -> assertFailure ("Parser failed: " ++ show err)
    Right project ->
      let expected =
            [ Action.ConvexFunction
                { Action.funcName = "getUser",
                  Action.funcPath = "myActions",
                  Action.funcType = Action.Query,
                  Action.funcArgs = [],
                  Action.funcReturn = Schema.VReference "UsersDoc"
                }
            ]
       in Parser.ppFunctions project @?= expected

testUnifyWithNestedObject :: Test
testUnifyWithNestedObject = "unifies function return with nested object" ~: TestCase $ do
  let schemaContent =
        unlines
          [ "import { defineSchema, defineTable } from \"convex/server\";",
            "import { v } from \"convex/values\";",
            "",
            "export default defineSchema({",
            "  users: defineTable({",
            "    name: v.string(),",
            "    profile: v.object({",
            "      image: v.string()",
            "    })",
            "  })",
            "});"
          ]
  let apiFileContent =
        unlines
          [ "declare const fullApi: ApiFromModules<{",
            "  \"myActions\": typeof myActions;",
            "}>;"
          ]
  let actionContent =
        unlines
          [ "export declare const getUser: import(\"convex/server\").RegisteredQuery<\"public\", {}, Promise<{ _id: import(\"convex/values\").GenericId<\"users\">; _creationTime: number; name: string; profile: { image: string; }; }>>;"
          ]
  let actionContents = [("myActions", actionContent)]

  result <- Parser.parseProjectFromContents schemaContent apiFileContent actionContents
  case result of
    Left err -> assertFailure ("Parser failed: " ++ show err)
    Right project ->
      let expected =
            [ Action.ConvexFunction
                { Action.funcName = "getUser",
                  Action.funcPath = "myActions",
                  Action.funcType = Action.Query,
                  Action.funcArgs = [],
                  Action.funcReturn = Schema.VReference "UsersDoc"
                }
            ]
       in Parser.ppFunctions project @?= expected

testUnifyWithArrayOfDocs :: Test
testUnifyWithArrayOfDocs = "unifies function returning an array of docs" ~: TestCase $ do
  let schemaContent =
        unlines
          [ "import { defineSchema, defineTable } from \"convex/server\";",
            "import { v } from \"convex/values\";",
            "",
            "export default defineSchema({",
            "  users: defineTable({",
            "    name: v.string(),",
            "    email: v.string()",
            "  })",
            "});"
          ]
  let apiFileContent =
        unlines
          [ "declare const fullApi: ApiFromModules<{",
            "  \"myActions\": typeof myActions;",
            "}>;"
          ]
  let actionContent =
        unlines
          [ "export declare const listUsers: import(\"convex/server\").RegisteredQuery<\"public\", {}, Promise<{",
            "  _id: import(\"convex/values\").GenericId<\"users\">;",
            "  _creationTime: number;",
            "  name: string;",
            "  email: string;",
            "}[]>>;"
          ]
  let actionContents = [("myActions", actionContent)]

  result <- Parser.parseProjectFromContents schemaContent apiFileContent actionContents
  case result of
    Left err -> assertFailure ("Parser failed: " ++ show err)
    Right project ->
      let expected =
            [ Action.ConvexFunction
                { Action.funcName = "listUsers",
                  Action.funcPath = "myActions",
                  Action.funcType = Action.Query,
                  Action.funcArgs = [],
                  Action.funcReturn = Schema.VArray (Schema.VReference "UsersDoc")
                }
            ]
       in Parser.ppFunctions project @?= expected

testUnifyWithDeeplyNestedObject :: Test
testUnifyWithDeeplyNestedObject = "unifies function return with deeply nested object" ~: TestCase $ do
  let schemaContent =
        unlines
          [ "import { defineSchema, defineTable } from \"convex/server\";",
            "import { v } from \"convex/values\";",
            "",
            "export default defineSchema({",
            "  users: defineTable({",
            "    name: v.string(),",
            "    profile: v.object({",
            "      image: v.string(),",
            "      details: v.object({",
            "        bio: v.string()",
            "      })",
            "    })",
            "  })",
            "});"
          ]
  let apiFileContent =
        unlines
          [ "declare const fullApi: ApiFromModules<{",
            "  \"myActions\": typeof myActions;",
            "}>;"
          ]
  let actionContent =
        unlines
          [ "export declare const getUser: import(\"convex/server\").RegisteredQuery<\"public\", {}, Promise<{ _id: import(\"convex/values\").GenericId<\"users\">; _creationTime: number; name: string; profile: { image: string; details: { bio: string; } }; }>>;"
          ]
  let actionContents = [("myActions", actionContent)]

  result <- Parser.parseProjectFromContents schemaContent apiFileContent actionContents
  case result of
    Left err -> assertFailure ("Parser failed: " ++ show err)
    Right project ->
      let expected =
            [ Action.ConvexFunction
                { Action.funcName = "getUser",
                  Action.funcPath = "myActions",
                  Action.funcType = Action.Query,
                  Action.funcArgs = [],
                  Action.funcReturn = Schema.VReference "UsersDoc"
                }
            ]
       in Parser.ppFunctions project @?= expected

tests :: Test
tests =
  "Unification"
    ~: test
      [ testUnifyWithTableDoc,
        testUnifyWithNestedObject,
        testUnifyWithArrayOfDocs,
        testUnifyWithDeeplyNestedObject,
        testUnifyWithShuffledFields
      ]

testUnifyWithShuffledFields :: Test
testUnifyWithShuffledFields = "unifies function return with shuffled fields" ~: TestCase $ do
  let schemaContent =
        unlines
          [ "import { defineSchema, defineTable } from \"convex/server\";",
            "import { v } from \"convex/values\";",
            "",
            "export default defineSchema({",
            "  users: defineTable({",
            "    name: v.string(),",
            "    email: v.string()",
            "  })",
            "});"
          ]
  let apiFileContent =
        unlines
          [ "declare const fullApi: ApiFromModules<{",
            "  \"myActions\": typeof myActions;",
            "}>;"
          ]
  let actionContent =
        unlines
          -- Note the shuffled fields: email, name instead of name, email
          [ "export declare const getUser: import(\"convex/server\").RegisteredQuery<\"public\", {}, Promise<{ _id: import(\"convex/values\").GenericId<\"users\">; _creationTime: number; email: string; name: string; }>>;"
          ]
  let actionContents = [("myActions", actionContent)]

  result <- Parser.parseProjectFromContents schemaContent apiFileContent actionContents
  case result of
    Left err -> assertFailure ("Parser failed: " ++ show err)
    Right project ->
      let expected =
            [ Action.ConvexFunction
                { Action.funcName = "getUser",
                  Action.funcPath = "myActions",
                  Action.funcType = Action.Query,
                  Action.funcArgs = [],
                  Action.funcReturn = Schema.VReference "UsersDoc"
                }
            ]
       in Parser.ppFunctions project @?= expected
