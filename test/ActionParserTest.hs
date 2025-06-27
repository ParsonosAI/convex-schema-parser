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
        runActionTest "ignores internal actions" "test/api" sampleInternalAction []
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
