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
  assertContains "payload[\"content\"] = _to_convex(self.content)" generated
  assertContains
    "TypeAdapter(list[Upsert_proofing_notesReturnVariant1Object | Upsert_proofing_notesReturnVariant2Object]).validate_python(raw_result)"
    generated

optionalSerializationFunction :: Action.ConvexFunction
optionalSerializationFunction =
  Action.ConvexFunction
    { Action.funcName = "update",
      Action.funcPath = "admin/actions",
      Action.funcType = Action.Mutation,
      Action.funcArgs =
        [ ("optional_count", Schema.VOptional Schema.VNumber),
          ("optional_note", Schema.VOptional (Schema.VUnion [Schema.VString, Schema.VNull])),
          ( "payload",
            Schema.VObject
              [ ("required_nullable", Schema.VUnion [Schema.VString, Schema.VNull]),
                ("optional_scalar", Schema.VOptional Schema.VString),
                ("optional_array", Schema.VOptional (Schema.VArray Schema.VString)),
                ("optional_nullable", Schema.VOptional (Schema.VUnion [Schema.VString, Schema.VNull])),
                ("optional_nullable_ref", Schema.VOptional (Schema.VReference "nullableString"))
              ]
          )
        ],
      Action.funcReturn = Schema.VVoid
    }

testOptionalAndNullableSerialization :: Test
testOptionalAndNullableSerialization = "preserves optional and nullable serialization semantics" ~: TestCase $ do
  let project =
        Convex.ParsedProject
          { Convex.ppConstants =
              Map.fromList
                [("nullableString", Schema.VUnion [Schema.VString, Schema.VNull])],
            Convex.ppSchema = Schema.Schema {Schema.getTables = []},
            Convex.ppFunctions = [optionalSerializationFunction]
          }
      generated = Python.generatePythonCode project

  assertContains "required_nullable: str | None = Field(...)" generated
  assertContains "optional_scalar: str | None = Field(default=None)" generated
  assertContains "optional_array: list[str] | None = Field(default=None)" generated
  assertContains "optional_nullable: str | None = Field(default=None)" generated
  assertContains "payload[\"required_nullable\"] = _to_convex(self.required_nullable)" generated
  assertContains "if self.optional_scalar is not None:" generated
  assertContains "payload[\"optional_scalar\"] = self.optional_scalar" generated
  assertContains "if self.optional_array is not None:" generated
  assertContains "payload[\"optional_array\"] = [item for item in self.optional_array]" generated
  assertContains "if self.optional_nullable is not None or \"optional_nullable\" in self.model_fields_set:" generated
  assertContains "if self.optional_nullable_ref is not None or \"optional_nullable_ref\" in self.model_fields_set:" generated
  assertContains "optional_count: float | None = None" generated
  assertContains "optional_note: str | None | UnsetType = UNSET" generated
  assertContains "if optional_count is not None:" generated
  assertContains "if not isinstance(optional_note, UnsetType):" generated
  assertBool
    "Optional arrays must not be serialized unconditionally"
    (not ("payload[\"optional_array\"] = [item for item in self.optional_array]\n        return payload" `isInfixOf` generated))

testMutuallyReferencingTableIds :: Test
testMutuallyReferencingTableIds = "generates mutually referencing table IDs" ~: TestCase $ do
  let project =
        Convex.ParsedProject
          { Convex.ppConstants = Map.empty,
            Convex.ppSchema =
              Schema.Schema
                { Schema.getTables =
                    [ Schema.Table
                        { Schema.tableName = "projects",
                          Schema.tableFields =
                            [ Schema.Field
                                "script_preparation_id"
                                (Schema.VOptional (Schema.VId "script_preparations"))
                            ],
                          Schema.tableIndexes = []
                        },
                      Schema.Table
                        { Schema.tableName = "script_preparations",
                          Schema.tableFields =
                            [Schema.Field "project_id" (Schema.VId "projects")],
                          Schema.tableIndexes = []
                        }
                    ]
                },
            Convex.ppFunctions = []
          }
      generated = Python.generatePythonCode project

  assertContains
    "script_preparation_id: Id['Script_preparationsDoc'] | None = Field(default=None)"
    generated
  assertContains "project_id: Id['ProjectsDoc'] = Field(...)" generated

tests :: Test
tests =
  "Python Serialization"
    ~: TestList
      [ testDistinctUnionModels,
        testOptionalAndNullableSerialization,
        testMutuallyReferencingTableIds
      ]
