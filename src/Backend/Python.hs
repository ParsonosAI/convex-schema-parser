{-# LANGUAGE OverloadedStrings #-}

module Backend.Python (generatePythonCode) where

import qualified Convex.Action.Parser as Action
import qualified Convex.Parser as P
import qualified Convex.Schema.Parser as Schema
import Data.Char (isUpper, toLower, toUpper)
import Data.List (intercalate, isPrefixOf, nub)
import qualified Data.Map as Map

indent :: Int -> String -> String
indent n s = replicate (n * 4) ' ' ++ s

generatePythonCode :: P.ParsedProject -> String
generatePythonCode project =
  let (nestedModelsFromFunctions, functionCode) = generateAllFunctions (P.ppFunctions project)
      (constantsCode, nestedModelsFromConstants) = generateAllConstants (P.ppConstants project)
      (schemaCode, nestedModelsFromSchema) = generateAllTables (P.ppSchema project)
      allNestedCode = unlines . nub $ nestedModelsFromSchema ++ nestedModelsFromFunctions ++ nestedModelsFromConstants
      aliasesCode = generateAliases (P.ppSchema project)
      apiClassCode = generateApiClass functionCode
   in unlines
        [ generateHeader,
          constantsCode,
          allNestedCode,
          schemaCode,
          aliasesCode,
          apiClassCode
        ]

-- | Generates the static header for the Python file.
generateHeader :: String
generateHeader =
  unlines
    [ "from pydantic import BaseModel, Field, ValidationError, TypeAdapter",
      "from typing import Any, Generic, TypeVar, Literal",
      "from pydantic_core import core_schema",
      "from convex import ConvexClient",
      "",
      "",
      "T = TypeVar('T')",
      "class Id(str, Generic[T]):",
      "    @classmethod",
      "    def __get_pydantic_core_schema__(cls, s, h) -> core_schema.CoreSchema:",
      "        return core_schema.no_info_after_validator_function(cls, core_schema.str_schema())",
      ""
    ]

-- | Generates Python type aliases for all the named constants.
generateAllConstants :: Map.Map String Schema.ConvexType -> (String, [String])
generateAllConstants constants =
  let results = map (generateConstant . fst) (Map.toList constants)
   in (unlines $ map fst results, concatMap snd results)
  where
    generateConstant name =
      let constType = constants Map.! name
          (pyType, _, _, nested) = toPythonTypeParts name constType
       in (name ++ " = " ++ pyType, nested)

-- | Generates Pydantic BaseModel classes for all tables.
generateAllTables :: Schema.Schema -> (String, [String])
generateAllTables (Schema.Schema tables) =
  let results = map generateTable tables
   in (unlines $ map fst results, concatMap snd results)

-- | Generates a single Pydantic BaseModel class for a table.
generateTable :: Schema.Table -> (String, [String])
generateTable table =
  let className = toClassName (Schema.tableName table)
      (fieldLines, nestedModels) = unzip $ map (generateField className) (Schema.tableFields table)
      tableCode =
        unlines
          [ "class " ++ className ++ "(BaseModel):",
            indent 1 ("_id: Id['" ++ className ++ "']"),
            indent 1 "_creationTime: float",
            unlines fieldLines
          ]
   in (tableCode, concat nestedModels)

-- | Generates singular type aliases for all table documents.
generateAliases :: Schema.Schema -> String
generateAliases (Schema.Schema tables) =
  let header = "\n# --- Singular Type Aliases for Ergonomics ---\n"
   in header ++ (unlines $ map toAlias tables)
  where
    toAlias t = toSingular (Schema.tableName t) ++ " = " ++ toClassName (Schema.tableName t)

-- | Generates a typed Python function for each Convex function.
generateAllFunctions :: [Action.ConvexFunction] -> ([String], String)
generateAllFunctions funcs =
  let results = map generateFunction funcs
   in (concatMap fst results, unlines $ map snd results)

-- | Generates the code for a single Python function wrapper.
generateFunction :: Action.ConvexFunction -> ([String], String)
generateFunction func =
  let funcName = Action.funcName func
      (argSignature, payloadMapping, nestedFromArgs) = generateArgSignature funcName (Action.funcArgs func)
      funcNameSnake = toSnakeCase funcName
      (rawReturnHint, isModelReturn, nestedFromReturn) = getReturnType funcName (Action.funcReturn func)

      handlerCall = case Action.funcType func of
        Action.Query -> "self.client.query"
        Action.Mutation -> "self.client.mutation"
        Action.Action -> "self.client.action"

      fullFuncPath = "\"" ++ Action.funcPath func ++ ":" ++ funcName ++ "\""

      (finalReturnHint, tryBlock) = case Action.funcReturn func of
        Schema.VVoid ->
          ( "None",
            unlines
              [ indent 3 (handlerCall ++ "(" ++ fullFuncPath ++ ", payload)"),
                indent 3 "return"
              ]
          )
        _ ->
          let hint = rawReturnHint ++ " | None"
              validationLogic =
                if isModelReturn
                  then
                    if "list[" `isPrefixOf` rawReturnHint
                      then "TypeAdapter(" ++ rawReturnHint ++ ").validate_python(raw_result)"
                      else rawReturnHint ++ ".model_validate(raw_result)"
                  else "raw_result"
              body =
                unlines
                  [ indent 3 ("raw_result = " ++ handlerCall ++ "(" ++ fullFuncPath ++ ", payload)"),
                    indent 3 "if raw_result is None:",
                    indent 4 "return None",
                    indent 3 ("return " ++ validationLogic)
                  ]
           in (hint, body)

      funcCode =
        unlines
          [ indent 1 ("def " ++ funcNameSnake ++ "(self, " ++ argSignature ++ ") -> " ++ finalReturnHint ++ ":"),
            indent 2 ("\"\"\"Wraps the " ++ fullFuncPath ++ " " ++ show (Action.funcType func) ++ ".\"\"\""),
            indent 2 ("payload: dict[str, Any] = {" ++ payloadMapping ++ "}"),
            indent 2 "try:",
            tryBlock,
            indent 2 "except ValidationError as e:",
            indent 3 ("print(f\"Validation error in '" ++ funcNameSnake ++ "': {e}\")"),
            indent 3 "raise",
            indent 2 "except Exception as e:",
            indent 3 ("print(f\"Error in '" ++ funcNameSnake ++ "': {e}\")"),
            indent 3 "raise",
            ""
          ]
   in (nestedFromArgs ++ nestedFromReturn, funcCode)

-- | Generates the `api` class that namespaces all the generated functions.
generateApiClass :: String -> String
generateApiClass functionCode =
  let header =
        [ "\n# --- API Client Class ---\n",
          "class API:",
          indent 1 "\"\"\"A type-safe client for your Convex API.\"\"\"",
          indent 1 "def __init__(self, client: ConvexClient):",
          indent 2 "self.client = client",
          ""
        ]
   in unlines (header ++ [functionCode])

-- Helper to generate Python function arguments and the payload dictionary mapping.
generateArgSignature :: String -> [(String, Schema.ConvexType)] -> (String, String, [String])
generateArgSignature funcName args =
  let results = map (\(n, t) -> (n, toPythonTypeParts (capitalize funcName ++ capitalize n) t)) args
      sigParts = map (\(n, (t, _, _, _)) -> toSnakeCase n ++ ": " ++ t) results
      payloadParts = map (\(n, _) -> "\"" ++ n ++ "\": " ++ toSnakeCase n) results
      nestedModels = concatMap (\(_, (_, _, _, n)) -> n) results
   in (intercalate ", " sigParts, intercalate ", " payloadParts, nestedModels)

-- Helper to get the return type information.
getReturnType :: String -> Schema.ConvexType -> (String, Bool, [String])
getReturnType funcName rt =
  let (pyType, _, _, nested) = toPythonTypeParts (capitalize funcName ++ "Return") rt
      isModel = case rt of
        Schema.VObject _ -> True
        Schema.VArray (Schema.VObject _) -> True
        Schema.VReference _ -> True
        _ -> False
   in (pyType, isModel, nested)

-- Helper to generate a single field line for a Pydantic model.
generateField :: String -> Schema.Field -> (String, [String])
generateField parentClassName field =
  let (pyType, isOpt, isArr, nested) = toPythonTypeParts (parentClassName ++ capitalize (Schema.fieldName field)) (Schema.fieldType field)
      defaultValue = case (isOpt, isArr) of
        (True, True) -> " = Field(default_factory=list)"
        (True, False) -> " = None"
        _ -> ""
   in (indent 1 (toSnakeCase (Schema.fieldName field) ++ ": " ++ pyType ++ defaultValue), nested)

-- Core recursive function to generate Python types from the AST.
toPythonTypeParts :: String -> Schema.ConvexType -> (String, Bool, Bool, [String])
toPythonTypeParts nameHint typ = case typ of
  Schema.VString -> ("str", False, False, [])
  Schema.VNumber -> ("float", False, False, [])
  Schema.VBoolean -> ("bool", False, False, [])
  Schema.VAny -> ("Any", False, False, [])
  Schema.VNull -> ("None", True, False, [])
  Schema.VId t -> ("Id['" ++ toClassName t ++ "']", False, False, [])
  Schema.VArray inner ->
    let (innerType, isOpt, isArr, nested) = toPythonTypeParts nameHint inner
     in ("list[" ++ innerType ++ "]", isOpt, isArr, nested)
  Schema.VOptional inner ->
    let (innerType, _, innerIsArray, nested) = toPythonTypeParts nameHint inner
     in (innerType ++ " | None", True, innerIsArray, nested)
  Schema.VUnion types ->
    let results = map (toPythonTypeParts nameHint) types
        pyTypes = nub $ map (\(t, _, _, _) -> t) results
        nested = concatMap (\(_, _, _, n) -> n) results
     in (intercalate " | " pyTypes, False, False, nested)
  Schema.VLiteral s -> ("Literal[\"" ++ s ++ "\"]", False, False, [])
  Schema.VReference n -> (n, False, False, [])
  Schema.VObject fields ->
    let className = capitalize nameHint ++ "Object"
        (fieldLines, nested) = unzip $ map (generateField className) (map (\(n, t) -> Schema.Field n t) fields)
        newModel = unlines $ ["class " ++ className ++ "(BaseModel):"] ++ fieldLines
     in (className, False, False, concat nested ++ [newModel])
  Schema.VVoid -> ("None", True, False, [])

capitalize :: String -> String
capitalize "" = ""
capitalize (c : cs) = toUpper c : cs

toSingular :: String -> String
toSingular s
  | last s == 's' = capitalize (init s)
  | otherwise = capitalize s

toClassName :: String -> String
toClassName s = capitalize s ++ "Doc"

toSnakeCase :: String -> String
toSnakeCase "" = ""
toSnakeCase (c : cs) = toLower c : go cs
  where
    go (c' : cs')
      | isUpper c' = '_' : toLower c' : go cs'
      | otherwise = c' : go cs'
    go "" = ""
