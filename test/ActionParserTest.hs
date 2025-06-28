{-# LANGUAGE OverloadedStrings #-}

module ActionParserTest (tests) where

import qualified Convex.Action.Parser as Action
import qualified Convex.Schema.Parser as Schema
import Test.HUnit
import Text.Parsec (runParserT)

tests :: Test
tests =
  "Action Definition Parser"
    ~: test
      [ runActionTest "parses createProject mutation" "test/api" sampleCreateProject expectedCreateProject,
        runActionTest "ignores internal actions" "test/api" sampleInternalAction [],
        runActionTest "parses public query with complex return" "functions/users" samplePublicGetOrgs expectedPublicGetOrgs,
        runActionTest "parses public action with optional args" "functions/stripe" samplePublicAction expectedPublicAction,
        runActionTest "parses internal action with external type as VAny" "functions/stripe" sampleStripeSubscriptionAction expectedStripeSubscriptionAction,
        runActionTest "parses public action with external type" "functions/stripe" sampleStripeCheckoutAction sampleStripeCheckoutActionExpected,
        runActionTest "parses public action with external type as VAny" "functions/stripe" sampleStripeSubscriptionActionPublic expectedStripeSubscriptionActionPublic
      ]
  where
    runActionTest testName path input expected =
      testName ~: TestCase $ do
        result <- runParserT (Action.parseActionFile path) Schema.initialState "(test)" input
        case result of
          Left err -> assertFailure ("Parser failed: " ++ show err)
          Right funcs -> funcs @?= expected

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

sampleInternalAction :: String
sampleInternalAction = "export declare const sendOtp: import(\"convex/server\").RegisteredAction<\"internal\", { email: string; }, Promise<{ success: boolean; }>>;"

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
