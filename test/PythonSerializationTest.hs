{-# LANGUAGE OverloadedStrings #-}

module PythonSerializationTest (tests) where

import qualified Backend.Python as Python
import qualified Convex.Action.Parser as Action
import qualified Convex.Parser as Convex
import qualified Convex.Schema.Parser as Schema
import Data.List (isInfixOf)
import qualified Data.Map as Map
import Test.HUnit

proofingUpsertFunction :: Action.ConvexFunction
proofingUpsertFunction =
  Action.ConvexFunction
    { Action.funcName = "upsert_proofing_notes",
      Action.funcPath = "admin/actions",
      Action.funcType = Action.Mutation,
      Action.funcArgs =
        [ ( "writes",
            Schema.VArray
              ( Schema.VObject
                  [ ("expected_revision", Schema.VUnion [Schema.VNumber, Schema.VNull]),
                    ( "note",
                      Schema.VObject
                        [ ( "content",
                            Schema.VUnion
                              [ Schema.VObject
                                  [ ("kind", Schema.VLiteral "localized"),
                                    ("value", Schema.VObject [("verbatim_text", Schema.VUnion [Schema.VString, Schema.VNull])])
                                  ],
                                Schema.VObject
                                  [ ("kind", Schema.VLiteral "consistency"),
                                    ("value", Schema.VObject [("subject", Schema.VString)])
                                  ]
                              ]
                          )
                        ]
                    )
                  ]
              )
          )
        ],
      Action.funcReturn =
        Schema.VArray
          ( Schema.VUnion
              [ Schema.VObject [("kind", Schema.VLiteral "consistency"), ("issue_id", Schema.VString)],
                Schema.VObject [("kind", Schema.VLiteral "localized"), ("issue_id", Schema.VString)]
              ]
          )
    }

assertContains :: String -> String -> Assertion
assertContains expected generated =
  assertBool
    ("Generated Python does not contain:\n\n" ++ expected ++ "\n\nGenerated code:\n" ++ generated)
    (expected `isInfixOf` generated)

testDistinctUnionModels :: Test
testDistinctUnionModels = "generates distinct models for object union branches" ~: TestCase $ do
  let project =
        Convex.ParsedProject
          { Convex.ppConstants = Map.empty,
            Convex.ppSchema = Schema.Schema {Schema.getTables = []},
            Convex.ppFunctions = [proofingUpsertFunction]
          }
      generated = Python.generatePythonCode project

  assertContains
    "class Upsert_proofing_notesWritesObjectNoteObjectContentVariant1Object(BaseModel):"
    generated
  assertContains
    "class Upsert_proofing_notesWritesObjectNoteObjectContentVariant2Object(BaseModel):"
    generated
  assertContains
    "content: Upsert_proofing_notesWritesObjectNoteObjectContentVariant1Object | Upsert_proofing_notesWritesObjectNoteObjectContentVariant2Object"
    generated
  assertContains "expected_revision: float | None = Field(...)" generated
  assertContains "verbatim_text: str | None = Field(...)" generated
  assertContains "\"content\" : _to_convex(self.content)" generated
  assertContains
    "TypeAdapter(list[Upsert_proofing_notesReturnVariant1Object | Upsert_proofing_notesReturnVariant2Object]).validate_python(raw_result)"
    generated

tests :: Test
tests = "Python Serialization" ~: TestList [testDistinctUnionModels]
