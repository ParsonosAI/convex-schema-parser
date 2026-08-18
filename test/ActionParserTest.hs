{-# LANGUAGE OverloadedStrings #-}

module ActionParserTest (tests) where

import qualified Convex.Action.Parser as Action
import qualified Convex.Parser as Parser
import qualified Convex.Schema.Parser as Schema
import qualified Data.Map as Map
import Test.HUnit
import Text.Parsec (runParserT)

-- A dedicated test runner for unification tests that first parses a schema.
runSchemaUnificationTest ::
  String ->
  String ->
  String ->
  [Action.ConvexFunction] ->
  Test
runSchemaUnificationTest testName schemaString actionString expected =
  testName ~: TestCase $ do
    -- First, parse the schema to get the constants.
    schemaResult <- Schema.parseSchema schemaString
    case schemaResult of
      Left err -> assertFailure ("Schema parser failed: " ++ show err)
      Right parsedSchemaFile -> do
        let initialState = Schema.ParserState {Schema.psConstants = Schema.parsedConstants parsedSchemaFile}
        -- Now, parse the action file with the constants from the schema.
        actionResult <- runParserT (Action.parseActionFile "testPath") initialState "(test)" actionString
        case actionResult of
          Left err -> assertFailure ("Action parser failed: " ++ show err)
          Right (funcs, _) ->
            let project =
                  Parser.ParsedProject
                    { Parser.ppSchema = Schema.parsedSchema parsedSchemaFile,
                      Parser.ppConstants = Schema.parsedConstants parsedSchemaFile,
                      Parser.ppFunctions = funcs
                    }
                unifiedProject = Parser.runUnificationPass project
             in Parser.ppFunctions unifiedProject @?= expected

-- A dedicated test runner for unification tests.
runUnificationTest ::
  String ->
  String ->
  Map.Map String Schema.ConvexType ->
  String ->
  [Action.ConvexFunction] ->
  Test
runUnificationTest testName path constants input expected =
  testName ~: TestCase $ do
    let initialState = Schema.ParserState {Schema.psConstants = constants}
    result <- runParserT (Action.parseActionFile path) initialState "(test)" input
    case result of
      Left err -> assertFailure ("Parser failed: " ++ show err)
      Right (funcs, _) ->
        let project =
              Parser.ParsedProject
                { Parser.ppSchema = Schema.Schema {Schema.getTables = []},
                  Parser.ppConstants = constants,
                  Parser.ppFunctions = funcs
                }
            unifiedProject = Parser.runUnificationPass project
         in Parser.ppFunctions unifiedProject @?= expected

runTypesParserTest ::
  String ->
  String ->
  [Action.DTSType] ->
  Test
runTypesParserTest testName input expected =
  testName ~: TestCase $ do
    result <- runParserT (Action.parseActionFile "testPath") initialState "(test)" input
    case result of
      Left err -> assertFailure ("Parser failed: " ++ show err)
      Right (_, types) -> types @?= expected
  where
    initialState = Schema.ParserState {Schema.psConstants = Map.empty}

tests :: Test
tests =
  "Action Definition Parser"
    ~: test
      [ runActionTest "parses createProject mutation" "test/api" sampleCreateProject expectedCreateProject,
        runActionTest "ignores internal actions" "test/api" sampleInternalAction [],
        runActionTest "ignores internal actions with external empty args" "test/api" sampleInternalEmptyArgs [],
        runActionTest "parses public query with complex return" "functions/users" samplePublicGetOrgs expectedPublicGetOrgs,
        runActionTest "parses public action with optional args" "functions/stripe" samplePublicAction expectedPublicAction,
        runActionTest "parses internal action with external type as VAny" "functions/stripe" sampleStripeSubscriptionAction expectedStripeSubscriptionAction,
        runActionTest "parses public action with external type" "functions/stripe" sampleStripeCheckoutAction sampleStripeCheckoutActionExpected,
        runActionTest "parses public action with external type as VAny" "functions/stripe" sampleStripeSubscriptionActionPublic expectedStripeSubscriptionActionPublic,
        runActionTest "parses createAsset mutation with complex args" "functions/assets" sampleCreateAssetAction expectedCreateAssetAction,
        runActionTest "parses intersection types in args" "test/api" sampleIntersectionAction expectedIntersectionAction,
        runActionTest "parses user query with structural return type" "functions/users" sampleQueryUsers expectedQueryUsers,
        runActionTest "parses update mutation with optional id arg" "test/api" sampleUpdateMutation expectedUpdateMutation,
        runActionTest "parses null in a return union" "test/api" sampleNullableReturn expectedNullableReturn,
        runActionTest "parses undefined in an optional union" "test/api" sampleOptionalUndefined expectedOptionalUndefined,
        runActionTest "parses optional arrays in unions" "test/api" sampleOptionalArrays expectedOptionalArrays,
        runActionTest "parses boolean literals and never arrays" "test/api" sampleBooleanAndNever expectedBooleanAndNever,
        runTypesParserTest "parses interface definition" sampleInterfaceDefinition expectedInterfaceFunction,
        runUnificationTest
          "unifies function return with named doc"
          "functions/users"
          userProfileConstants
          sampleGetUserAction
          expectedGetUserAction,
        runSchemaUnificationTest
          "unifies function return with table doc"
          sampleSchemaWithUserTable
          sampleGetUserTableAction
          expectedGetUserTableAction
      ]
  where
    runActionTest testName path input expected =
      testName ~: TestCase $ do
        result <- runParserT (Action.parseActionFile path) Schema.initialState "(test)" input
        case result of
          Left err -> assertFailure ("Parser failed: " ++ show err)
          Right (funcs, _) -> funcs @?= expected

sampleCreateProject :: String
sampleCreateProject =
  unlines
    [ "export declare const createProject: import(\"convex/server\").RegisteredMutation<\"public\", {",
      "  project_name?: string;",
      "  tenant_id: import(\"convex/values\").GenericId<\"tenants\">;",
      "}, Promise<import(\"convex/values\").GenericId<\"projects\">>>;"
    ]

expectedCreateProject :: [Action.ConvexFunction]
expectedCreateProject =
  [ Action.ConvexFunction
      { Action.funcName = "createProject",
        Action.funcPath = "test/api",
        Action.funcType = Action.Mutation,
        Action.funcArgs =
          [ ("project_name", Schema.VOptional Schema.VString),
            ("tenant_id", Schema.VId "tenants")
          ],
        Action.funcReturn = Schema.VId "projects"
      }
  ]

sampleIntersectionAction :: String
sampleIntersectionAction =
  unlines
    [ "export declare const createProject: import(\"convex/server\").RegisteredMutation<\"public\", {",
      "    isbn?: string & {",
      "        _: \"isbn\";",
      "    };",
      "    project_name?: string;",
      "    tenant_id: import(\"convex/values\").GenericId<\"tenants\">;",
      "}, Promise<import(\"convex/values\").GenericId<\"projects\">>>;"
    ]

expectedIntersectionAction :: [Action.ConvexFunction]
expectedIntersectionAction =
  [ Action.ConvexFunction
      { Action.funcName = "createProject",
        Action.funcPath = "test/api",
        Action.funcType = Action.Mutation,
        Action.funcArgs =
          [ ("isbn", Schema.VOptional Schema.VString),
            ("project_name", Schema.VOptional Schema.VString),
            ("tenant_id", Schema.VId "tenants")
          ],
        Action.funcReturn = Schema.VId "projects"
      }
  ]

sampleNullableReturn :: String
sampleNullableReturn =
  "export declare const getRevision: import(\"convex/server\").RegisteredQuery<\"public\", {}, Promise<number | null>>;"

expectedNullableReturn :: [Action.ConvexFunction]
expectedNullableReturn =
  [ Action.ConvexFunction
      { Action.funcName = "getRevision",
        Action.funcPath = "test/api",
        Action.funcType = Action.Query,
        Action.funcArgs = [],
        Action.funcReturn = Schema.VUnion [Schema.VNumber, Schema.VNull]
      }
  ]

sampleOptionalUndefined :: String
sampleOptionalUndefined =
  "export declare const update: import(\"convex/server\").RegisteredMutation<\"public\", { valid?: boolean | undefined; }, Promise<void>>;"

expectedOptionalUndefined :: [Action.ConvexFunction]
expectedOptionalUndefined =
  [ Action.ConvexFunction
      { Action.funcName = "update",
        Action.funcPath = "test/api",
        Action.funcType = Action.Mutation,
        Action.funcArgs =
          [("valid", Schema.VOptional Schema.VBoolean)],
        Action.funcReturn = Schema.VVoid
      }
  ]

sampleOptionalArrays :: String
sampleOptionalArrays =
  "export declare const update: import(\"convex/server\").RegisteredMutation<\"public\", { aliases?: string[] | undefined; kinds?: (\"one\" | \"two\")[] | undefined; }, Promise<void>>;"

expectedOptionalArrays :: [Action.ConvexFunction]
expectedOptionalArrays =
  [ Action.ConvexFunction
      { Action.funcName = "update",
        Action.funcPath = "test/api",
        Action.funcType = Action.Mutation,
        Action.funcArgs =
          [ ("aliases", Schema.VOptional (Schema.VArray Schema.VString)),
            ( "kinds",
              Schema.VOptional
                (Schema.VArray (Schema.VUnion [Schema.VLiteral "one", Schema.VLiteral "two"]))
            )
          ],
        Action.funcReturn = Schema.VVoid
      }
  ]

sampleBooleanAndNever :: String
sampleBooleanAndNever =
  "export declare const page: import(\"convex/server\").RegisteredQuery<\"public\", {}, Promise<{ active: false; entries: never[]; } | { active: true; entries: string[]; }>>;"

expectedBooleanAndNever :: [Action.ConvexFunction]
expectedBooleanAndNever =
  [ Action.ConvexFunction
      { Action.funcName = "page",
        Action.funcPath = "test/api",
        Action.funcType = Action.Query,
        Action.funcArgs = [],
        Action.funcReturn =
          Schema.VUnion
            [ Schema.VObject [("active", Schema.VBoolean), ("entries", Schema.VArray Schema.VAny)],
              Schema.VObject [("active", Schema.VBoolean), ("entries", Schema.VArray Schema.VString)]
            ]
      }
  ]

sampleInternalAction :: String
sampleInternalAction = "export declare const sendOtp: import(\"convex/server\").RegisteredAction<\"internal\", { email: string; }, Promise<{ success: boolean; }>>;"

sampleInternalEmptyArgs :: String
sampleInternalEmptyArgs = "export declare const cleanup: import(\"convex/dist/esm-types/server\").RegisteredMutation<\"internal\", import(\"convex/dist/esm-types/server/registration\").EmptyObject, Promise<Record<string, unknown>>>;"

samplePublicGetOrgs :: String
samplePublicGetOrgs =
  unlines
    [ "export declare const getOrgs: import(\"convex/server\").RegisteredQuery<\"public\", {}, Promise<{",
      "  name: string;",
      "  _id: import(\"convex/values\").GenericId<\"memberships\">;",
      "  _creationTime: number;",
      "  user_id: import(\"convex/values\").GenericId<\"users\">;",
      "  tenant_id: import(\"convex/values\").GenericId<\"tenants\">;",
      "  roles: (\"viewer\" | \"user\" | \"admin\")[];",
      "}[]>>;"
    ]

expectedPublicGetOrgs :: [Action.ConvexFunction]
expectedPublicGetOrgs =
  [ Action.ConvexFunction
      { Action.funcName = "getOrgs",
        Action.funcPath = "functions/users",
        Action.funcType = Action.Query,
        Action.funcArgs = [],
        Action.funcReturn =
          Schema.VArray
            ( Schema.VObject
                [ ("name", Schema.VString),
                  ("_id", Schema.VId "memberships"),
                  ("_creationTime", Schema.VNumber),
                  ("user_id", Schema.VId "users"),
                  ("tenant_id", Schema.VId "tenants"),
                  ( "roles",
                    Schema.VArray
                      ( Schema.VUnion
                          [ Schema.VLiteral "viewer",
                            Schema.VLiteral "user",
                            Schema.VLiteral "admin"
                          ]
                      )
                  )
                ]
            )
      }
  ]

samplePublicAction :: String
samplePublicAction =
  unlines
    [ "export declare const createCheckoutSession: import(\"convex/server\").RegisteredAction<\"public\", {",
      "    next_url?: string;",
      "    tenant_id: import(\"convex/values\").GenericId<\"tenants\">;",
      "    price_id: string;",
      "}, Promise<string>>;"
    ]

expectedPublicAction :: [Action.ConvexFunction]
expectedPublicAction =
  [ Action.ConvexFunction
      { Action.funcName = "createCheckoutSession",
        Action.funcPath = "functions/stripe",
        Action.funcType = Action.Action,
        Action.funcArgs =
          [ ("next_url", Schema.VOptional Schema.VString),
            ("tenant_id", Schema.VId "tenants"),
            ("price_id", Schema.VString)
          ],
        Action.funcReturn = Schema.VString
      }
  ]

sampleStripeSubscriptionAction :: String
sampleStripeSubscriptionAction = "export declare const stripeSubscription: import(\"convex/server\").RegisteredAction<\"internal\", { subscription_id: string; }, Promise<Stripe.Subscription>>;"

expectedStripeSubscriptionAction :: [Action.ConvexFunction]
expectedStripeSubscriptionAction = []

sampleStripeSubscriptionActionPublic :: String
sampleStripeSubscriptionActionPublic =
  unlines
    [ "export declare const stripeSubscription: import(\"convex/server\").RegisteredAction<\"public\", {",
      "    subscription_id: string;",
      "}, Promise<Stripe.Subscription>>;"
    ]

-- Even though the return type is an external type, it should still parse correctly.
expectedStripeSubscriptionActionPublic :: [Action.ConvexFunction]
expectedStripeSubscriptionActionPublic =
  [ Action.ConvexFunction
      { Action.funcName = "stripeSubscription",
        Action.funcPath = "functions/stripe",
        Action.funcType = Action.Action,
        Action.funcArgs = [("subscription_id", Schema.VString)],
        Action.funcReturn = Schema.VAny -- External type, parsed as VAny
      }
  ]

sampleStripeCheckoutAction :: String
sampleStripeCheckoutAction =
  unlines
    [ "export declare const createCheckoutSession: import(\"convex/server\").RegisteredAction<\"public\", {",
      "    next_url?: string;",
      "    tenant_id: import(\"convex/values\").GenericId<\"tenants\">;",
      "    price_id: string;",
      "}, Promise<string>>;"
    ]

sampleStripeCheckoutActionExpected :: [Action.ConvexFunction]
sampleStripeCheckoutActionExpected =
  [ Action.ConvexFunction
      { Action.funcName = "createCheckoutSession",
        Action.funcPath = "functions/stripe",
        Action.funcType = Action.Action,
        Action.funcArgs =
          [ ("next_url", Schema.VOptional Schema.VString),
            ("tenant_id", Schema.VId "tenants"),
            ("price_id", Schema.VString)
          ],
        Action.funcReturn = Schema.VString
      }
  ]

sampleCreateAssetAction :: String
sampleCreateAssetAction =
  unlines
    [ "export declare const createAsset: import(\"convex/server\").RegisteredMutation<\"public\", {",
      "    projectId: import(\"convex/values\").GenericId<\"projects\">;",
      "    linkMetadata: {",
      "        summary: ArrayBuffer;",
      "        sample_rate: number;",
      "        length: number;",
      "        linkable_uri: string;",
      "    };",
      "}, Promise<import(\"convex/values\").GenericId<\"assets\">>>;"
    ]

expectedCreateAssetAction :: [Action.ConvexFunction]
expectedCreateAssetAction =
  [ Action.ConvexFunction
      { Action.funcName = "createAsset",
        Action.funcPath = "functions/assets",
        Action.funcType = Action.Mutation,
        Action.funcArgs =
          [ ("projectId", Schema.VId "projects"),
            ( "linkMetadata",
              Schema.VObject
                [ ("summary", Schema.VBytes),
                  ("sample_rate", Schema.VNumber),
                  ("length", Schema.VNumber),
                  ("linkable_uri", Schema.VString)
                ]
            )
          ],
        Action.funcReturn = Schema.VId "assets"
      }
  ]

-- Test case for structural unification of a function's return type.
userProfileConstants :: Map.Map String Schema.ConvexType
userProfileConstants =
  Map.fromList
    [ ("UserProfile", userProfileType),
      ("Address", addressType)
    ]
  where
    addressType =
      Schema.VObject
        [ ("street", Schema.VString),
          ("city", Schema.VString)
        ]
    userProfileType =
      Schema.VObject
        [ ("name", Schema.VString),
          ("address", Schema.VReference "Address")
        ]

sampleGetUserAction :: String
sampleGetUserAction =
  unlines
    [ "export declare const getUser: import(\"convex/server\").RegisteredQuery<\"public\", {}, Promise<{",
      "  name: string;",
      "  address: {",
      "    street: string;",
      "    city: string;",
      "  };",
      "}>>;"
    ]

expectedGetUserAction :: [Action.ConvexFunction]
expectedGetUserAction =
  [ Action.ConvexFunction
      { Action.funcName = "getUser",
        Action.funcPath = "functions/users",
        Action.funcType = Action.Query,
        Action.funcArgs = [],
        Action.funcReturn = Schema.VReference "UserProfile" -- Should be unified
      }
  ]

-- Test case for unification against a table doc.
sampleSchemaWithUserTable :: String
sampleSchemaWithUserTable =
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

sampleGetUserTableAction :: String
sampleGetUserTableAction =
  unlines
    [ "export declare const getUser: import(\"convex/server\").RegisteredQuery<\"public\", {}, Promise<{",
      "  _id: import(\"convex/values\").GenericId<\"users\">;",
      "  _creationTime: number;",
      "  name: string;",
      "  email: string;",
      "}>>;"
    ]

expectedGetUserTableAction :: [Action.ConvexFunction]
expectedGetUserTableAction =
  [ Action.ConvexFunction
      { Action.funcName = "getUser",
        Action.funcPath = "testPath",
        Action.funcType = Action.Query,
        Action.funcArgs = [],
        Action.funcReturn = Schema.VReference "UsersDoc" -- Should be unified
      }
  ]

sampleSchemaWithAssetsTable :: String
sampleSchemaWithAssetsTable =
  unlines
    [ "import { defineSchema, defineTable } from \"convex/server\";",
      "import { v } from \"convex/values\";",
      "",
      "export default defineSchema({",
      "  projects: defineTable({",
      "    name: v.string()",
      "  }),",
      "  assets: defineTable({",
      "    project_id: v.id(\"projects\"),",
      "    asset_name: v.string(),",
      "    asset_essence_mtime: v.number(),",
      "    link_metadata: v.object({",
      "      sample_rate: v.number(),",
      "      summary: v.bytes(),",
      "      length: v.number()",
      "    })",
      "  })",
      "});"
    ]

sampleGetAssetsAction :: String
sampleGetAssetsAction =
  unlines
    [ "export declare const getAssets: import(\"convex/server\").RegisteredQuery<\"public\", {",
      "  project_id: import(\"convex/values\").Id<\"projects\">;",
      "  secret: string;",
      "}, Promise<Array<{",
      "  _id: import(\"convex/values\").Id<\"assets\">;",
      "  _creationTime: number;",
      "  project_id: import(\"convex/values\").Id<\"projects\">;",
      "  asset_name: string;",
      "  asset_essence_mtime: number;",
      "  link_metadata: {",
      "    summary: Uint8Array;",
      "    sample_rate: number;",
      "    length: number;",
      "  };",
      "}>>>;"
    ]

expectedGetAssetsAction :: [Action.ConvexFunction]
expectedGetAssetsAction =
  [ Action.ConvexFunction
      { Action.funcName = "getAssets",
        Action.funcPath = "testPath",
        Action.funcType = Action.Query,
        Action.funcArgs =
          [ ("project_id", Schema.VId "projects"),
            ("secret", Schema.VString)
          ],
        Action.funcReturn = Schema.VArray (Schema.VReference "AssetsDoc")
      }
  ]

sampleInterfaceDefinition :: String
sampleInterfaceDefinition =
  unlines
    [ "/**",
      " * This is a sample interface definition for testing.",
      " */",
      "export interface User {",
      "  // The user's name",
      "  name: string;",
      "  // The user's email address",
      "  email: string;",
      "}",
      "",
      "import { somethingElse } from \"somewhere\"; // This should be ignored"
    ]

expectedInterfaceFunction :: [Action.DTSType]
expectedInterfaceFunction =
  [ Action.DTSType
      { Action.dtsTypeName = "User",
        Action.dtsTypeFields =
          [ ("name", Schema.VString),
            ("email", Schema.VString)
          ]
      }
  ]

sampleQueryUsers :: String
sampleQueryUsers =
  unlines
    [ "export declare const getOrgs: import(\"convex/server\").RegisteredQuery<\"public\", {}, Promise<{",
      "    name: string;",
      "    _id: import(\"convex/values\").GenericId<\"memberships\">;",
      "    _creationTime: number;",
      "    user_id: import(\"convex/values\").GenericId<\"users\">;",
      "    tenant_id: import(\"convex/values\").GenericId<\"tenants\">;",
      "    roles: (\"viewer\" | \"user\" | \"admin\")[];",
      "}[]>>;"
    ]

expectedQueryUsers :: [Action.ConvexFunction]
expectedQueryUsers =
  [ Action.ConvexFunction
      { Action.funcName = "getOrgs",
        Action.funcPath = "functions/users",
        Action.funcType = Action.Query,
        Action.funcArgs = [],
        Action.funcReturn =
          Schema.VArray
            ( Schema.VObject
                [ ("name", Schema.VString),
                  ("_id", Schema.VId "memberships"),
                  ("_creationTime", Schema.VNumber),
                  ("user_id", Schema.VId "users"),
                  ("tenant_id", Schema.VId "tenants"),
                  ( "roles",
                    Schema.VArray
                      ( Schema.VUnion
                          [ Schema.VLiteral "viewer",
                            Schema.VLiteral "user",
                            Schema.VLiteral "admin"
                          ]
                      )
                  )
                ]
            )
      }
  ]

sampleUpdateMutation :: String
sampleUpdateMutation =
  unlines
    [ "export declare const update_and_create_group: import(\"convex/dist/esm-types/server\").RegisteredMutation<\"public\", {",
      "    group_id?: import(\"convex/dist/esm-types/values\").GenericId<\"asset_groups\">;",
      "    name: string;",
      "    project_id: import(\"convex/dist/esm-types/values\").GenericId<\"projects\">;",
      "    sort_index: number;",
      "    secret: string;",
      "}, Promise<Id<\"asset_groups\">>>;"
    ]

expectedUpdateMutation :: [Action.ConvexFunction]
expectedUpdateMutation =
  [ Action.ConvexFunction
      { Action.funcName = "update_and_create_group",
        Action.funcPath = "test/api",
        Action.funcType = Action.Mutation,
        Action.funcArgs =
          [ ("group_id", Schema.VOptional (Schema.VId "asset_groups")),
            ("name", Schema.VString),
            ("project_id", Schema.VId "projects"),
            ("sort_index", Schema.VNumber),
            ("secret", Schema.VString)
          ],
        Action.funcReturn = Schema.VId "asset_groups"
      }
  ]
