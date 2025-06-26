{-# LANGUAGE OverloadedStrings #-}

module Backend.Python
  ( generatePythonCode,
  )
where

import qualified Convex.Schema.Parser as AST
import qualified Data.Char as Ch
import Data.List (intercalate, nub)
import qualified Data.Map as Map

generatePythonCode :: AST.ParsedFile -> String
generatePythonCode parsed =
  let -- We will assume conventional naming and strip the last `s` in table names
      -- to generate class names.
      -- First, generate the table models and collect all nested models that are created.
      (tableCode, nestedModels) = generateAllTables $ AST.parsedSchema parsed
      -- Also collect any nested models from the top-level constants.
      (constantsCode, nestedFromConstants) = generateAllConstants $ AST.parsedConstants parsed
      -- Combine and deduplicate all nested model definitions.
      allNestedCode = unlines . nub $ nestedModels ++ nestedFromConstants
   in unlines
        [ generateHeader,
          constantsCode,
          allNestedCode,
          tableCode
        ]

-- | Generates the static header for the Python file.
generateHeader :: String
generateHeader =
  unlines
    [ "from pydantic import BaseModel, Field",
      "from typing import Any, Generic, TypeVar, Literal",
      "from pydantic_core import core_schema",
      "",
      "T = TypeVar('T')",
      "",
      "class Id(str, Generic[T]):",
      "    \"\"\"A Pydantic-compatible wrapper for Convex document IDs that behaves like a string.\"\"\"",
      "",
      "    @classmethod",
      "    def __get_pydantic_core_schema__(",
      "        cls, source_type: Any, handler: Any",
      "    ) -> core_schema.CoreSchema:",
      "        return core_schema.no_info_after_validator_function(cls, core_schema.str_schema())",
      ""
    ]

generateAllConstants :: Map.Map String AST.ConvexType -> (String, [String])
generateAllConstants constants =
  let results = map (generateConstant . fst) (Map.toList constants)
      code = unlines $ map fst results
      nested = concatMap snd results
   in (code, nested)
  where
    generateConstant :: String -> (String, [String])
    generateConstant name =
      let constType = constants Map.! name
          (pyType, _, _, nestedModels) = toPythonTypeParts name constType
       in (name ++ " = " ++ pyType, nestedModels)

generateAllTables :: AST.Schema -> (String, [String])
generateAllTables (AST.Schema tables) =
  let results = map generateTable tables
      tableCode = unlines $ map fst results
      nestedModels = concatMap snd results
   in (tableCode, nestedModels)

generateTable :: AST.Table -> (String, [String])
generateTable table =
  let className = capitalize (AST.tableName table)
      (fieldLines, nestedModels) = unzip $ map (generateField className) (AST.tableFields table)
      tableCode =
        unlines
          [ "class " ++ className ++ "(BaseModel):",
            "    \"\"\"Document type for the '" ++ AST.tableName table ++ "' table. Validated by Pydantic.\"\"\"",
            "    _id: Id['" ++ className ++ "']",
            "    _creationTime: float",
            unlines fieldLines
          ]
   in (tableCode, concat nestedModels)

capitalize :: String -> String
capitalize "" = ""
capitalize (c : cs) = Ch.toUpper c : cs

generateField :: String -> AST.Field -> (String, [String])
generateField parentClassName field =
  let (pythonType, isOptional, isArray, nestedModels) = toPythonTypeParts (parentClassName ++ capitalize (AST.fieldName field)) (AST.fieldType field)
      defaultValue = case (isOptional, isArray) of
        (True, True) -> " = Field(default_factory=list)"
        (True, False) -> " = None"
        (False, _) -> ""
      fieldLine = "    " ++ AST.fieldName field ++ ": " ++ pythonType ++ defaultValue
   in (fieldLine, nestedModels)

toPythonTypeParts :: String -> AST.ConvexType -> (String, Bool, Bool, [String])
toPythonTypeParts nameHint typ = case typ of
  AST.VString -> ("str", False, False, [])
  AST.VNumber -> ("float", False, False, [])
  AST.VBoolean -> ("bool", False, False, [])
  AST.VAny -> ("Any", False, False, [])
  AST.VNull -> ("None", True, False, [])
  AST.VId table -> ("Id['" ++ capitalize table ++ "']", False, False, [])
  AST.VArray inner ->
    let (innerType, isOpt, isArr, nested) = toPythonTypeParts nameHint inner
     in ("list[" ++ innerType ++ "]", isOpt, isArr, nested)
  AST.VOptional inner ->
    let (innerType, _, innerIsArray, nested) = toPythonTypeParts nameHint inner
     in (innerType ++ " | None", True, innerIsArray, nested)
  AST.VUnion types ->
    let results = map (toPythonTypeParts nameHint) types
        pyTypes = map (\(t, _, _, _) -> t) results
        nested = concatMap (\(_, _, _, n) -> n) results
     in (intercalate " | " pyTypes, False, False, nested)
  AST.VLiteral s -> ("Literal['" ++ s ++ "']", False, False, [])
  AST.VReference name -> (name, False, False, [])
  AST.VObject fields ->
    let className = nameHint ++ "Object"
        (fieldLines, nestedFromFields) = unzip $ map (generateField className . tupleToField) fields
        newModelCode =
          unlines
            [ "class " ++ className ++ "(BaseModel):",
              "    \"\"\"Nested object structure.\"\"\"",
              unlines fieldLines
            ]
     in (className, False, False, newModelCode : concat nestedFromFields)

tupleToField :: (String, AST.ConvexType) -> AST.Field
tupleToField (name, typ) =
  AST.Field
    { AST.fieldName = name,
      AST.fieldType = typ
    }
