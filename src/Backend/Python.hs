{-# LANGUAGE OverloadedStrings #-}

module Backend.Python (generatePythonCode) where

import qualified Convex.Action.Parser as Action
import qualified Convex.Parser as P
import qualified Convex.Schema.Parser as Schema
import Data.Char (isUpper, toLower, toUpper)
import Data.List (intercalate, isPrefixOf, nub)
import qualified Data.Map.Strict as Map
import PathTree

-- Helper function to prepend a given number of spaces (4 per level).
indent :: Int -> String -> String
indent n s = replicate (n * 4) ' ' ++ s

generatePythonCode :: P.ParsedProject -> String
generatePythonCode project =
  let (constantsCode, nestedModelsFromConstants) = generateAllConstants (P.ppConstants project)
      (schemaCode, nestedFromFields) = generateAllTables (P.ppSchema project)
      (apiClassCode, nestedFromFuncs) = generateApiClass (P.ppFunctions project)
      allNestedCode = unlines . nub $ nestedFromFields ++ nestedModelsFromConstants ++ nestedFromFuncs
      aliasesCode = generateAliases (P.ppSchema project)
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
    [ "from typing import Any, Generic, Iterator, Literal, TypeVar",
      "",
      "from convex import ConvexClient",
      "from pydantic import BaseModel, TypeAdapter, ValidationError",
      "from pydantic_core import core_schema",
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
  let (tableCodes, nested) = unzip $ map generateTable tables
   in (unlines tableCodes, concat nested)

-- | Generates a single Pydantic BaseModel class for a table.
generateTable :: Schema.Table -> (String, [String])
generateTable table =
  let className = toClassName (Schema.tableName table)
      (fieldLines, nestedModelsFromFields) = unzip $ map (generateField className) (Schema.tableFields table)
      tableCode =
        unlines
          [ "class " ++ className ++ "(BaseModel):",
            indent 1 ("_id: Id['" ++ className ++ "']"),
            indent 1 "_creationTime: float",
            unlines fieldLines
          ]
   in (tableCode, concat nestedModelsFromFields)

-- | Generates singular type aliases for all table documents.
generateAliases :: Schema.Schema -> String
generateAliases (Schema.Schema tables) =
  let header = "\n# --- Singular Type Aliases for Ergonomics ---\n"
   in header ++ (unlines $ map toAlias tables)
  where
    toAlias t = toSingular (Schema.tableName t) ++ " = " ++ toClassName (Schema.tableName t)

-- | Generates the code for a single Python function wrapper.
generateFunction :: Int -> Action.ConvexFunction -> (String, [String])
generateFunction level func =
  let funcName = Action.funcName func
      (argSignature, payloadMapping, nestedFromArgs) = generateArgSignature funcName (Action.funcArgs func)
      funcNameSnake = toSnakeCase funcName
      (rawReturnHint, isModelReturn, nestedFromReturn) = getReturnType funcName (Action.funcReturn func)

      handlerCall = case Action.funcType func of
        Action.Query -> "self._client.query"
        Action.Mutation -> "self._client.mutation"
        Action.Action -> "self._client.action"

      fullFuncPath = "\"" ++ Action.funcPath func ++ ":" ++ funcName ++ "\""

      (finalReturnHint, tryBlock) = case Action.funcReturn func of
        Schema.VVoid ->
          ( "None",
            unlines
              [ indent (level + 2) (handlerCall ++ "(" ++ fullFuncPath ++ ", payload)"),
                indent (level + 2) "return"
              ]
          )
        _ ->
          let hint = rawReturnHint ++ " | None"
              rawResultDeclaration =
                if isModelReturn
                  then "raw_result = "
                  else "raw_result: " ++ hint ++ " = "
              validationLogic =
                if isModelReturn
                  then
                    if "list[" `isPrefixOf` rawReturnHint
                      then "TypeAdapter(" ++ rawReturnHint ++ ").validate_python(raw_result)"
                      else rawReturnHint ++ ".model_validate(raw_result)"
                  else "raw_result"
              body =
                unlines
                  [ indent (level + 2) (rawResultDeclaration ++ handlerCall ++ "(" ++ fullFuncPath ++ ", payload)"),
                    indent (level + 2) "if raw_result is None:",
                    indent (level + 3) "return None",
                    indent (level + 2) ("return " ++ validationLogic)
                  ]
           in (hint, body)

      funcCode =
        unlines
          [ indent level ("def " ++ funcNameSnake ++ "(self, " ++ argSignature ++ ") -> " ++ finalReturnHint ++ ":"),
            indent (level + 1) ("\"\"\"Wraps the " ++ fullFuncPath ++ " " ++ show (Action.funcType func) ++ ".\"\"\""),
            indent (level + 1) ("payload: dict[str, Any] = {" ++ payloadMapping ++ "}"),
            indent (level + 1) "try:",
            tryBlock,
            indent (level + 1) "except ValidationError as e:",
            indent (level + 2) ("print(f\"Validation error in '" ++ funcNameSnake ++ "': {e}\")"),
            indent (level + 2) "raise",
            indent (level + 1) "except Exception as e:",
            indent (level + 2) ("print(f\"Error in '" ++ funcNameSnake ++ "': {e}\")"),
            indent (level + 2) "raise"
          ]
   in (funcCode, nestedFromArgs ++ nestedFromReturn)

generateSubscriptionFunction :: Int -> Action.ConvexFunction -> (String, [String])
generateSubscriptionFunction level func =
  let funcName = Action.funcName func
      (argSignature, payloadMapping, nestedFromArgs) = generateArgSignature funcName (Action.funcArgs func)
      funcNameSnake = "subscribe_" ++ toSnakeCase funcName
      (returnHint, _, nestedFromReturn) = getReturnType funcName (Action.funcReturn func)
      finalReturnHint = "Iterator[" ++ returnHint ++ "]"
      fullFuncPath = "\"" ++ Action.funcPath func ++ ":" ++ funcName ++ "\""

      -- We create a TypeAdapter for ALL subscription functions, this also handles primitive types.
      adapterCreation = indent (level + 1) ("adapter = TypeAdapter(" ++ returnHint ++ ")")
      validationLogic = indent (level + 3) "validated_result = adapter.validate_python(raw_result)"

      funcCode =
        unlines
          [ indent level ("def " ++ funcNameSnake ++ "(self, " ++ argSignature ++ ") -> " ++ finalReturnHint ++ ":"),
            indent (level + 1) ("\"\"\"Subscribes to the " ++ fullFuncPath ++ " query.\"\"\""),
            indent (level + 1) ("payload: dict[str, Any] = {" ++ payloadMapping ++ "}"),
            indent (level + 1) ("raw_subscription = self._client.subscribe(" ++ fullFuncPath ++ ", payload)"),
            adapterCreation,
            indent (level + 1) "for raw_result in raw_subscription:",
            indent (level + 2) "try:",
            validationLogic,
            indent (level + 3) "yield validated_result",
            indent (level + 2) "except ValidationError as e:",
            indent (level + 3) "print(f\"Validation error in subscription update: {e}\")",
            indent (level + 3) "continue"
          ]
   in (funcCode, nestedFromArgs ++ nestedFromReturn)

generateApiStructure :: Int -> PathTree -> ([String], [String], [String])
generateApiStructure level (DirNode dir) =
  let (inits, defs, nested) = foldl processEntry ([], [], []) (Map.toList dir)
   in (inits, defs, nested)
  where
    processEntry (is, ds, ns) (_name, FuncNode func) =
      let (funcDef, nestedFromFunc) = generateFunction level func
          (subDef, nestedFromSub) =
            if Action.funcType func == Action.Query
              then generateSubscriptionFunction level func
              else ("", [])
       in (is, ds ++ [funcDef, subDef], ns ++ nestedFromFunc ++ nestedFromSub)
    processEntry (is, ds, ns) (name, DirNode subDir) =
      let className = capitalize name
          attrName = toSnakeCase name
          initLine = "self." ++ attrName ++ " = self." ++ className ++ "(self._client)"
          (subInits, subDefs, nestedFromSub) = generateApiStructure (level + 1) (DirNode subDir)
          classDef =
            unlines $
              [ "",
                indent level ("class " ++ className ++ ":"),
                indent (level + 1) "def __init__(self, client: ConvexClient):",
                indent (level + 2) "self._client = client"
              ]
                ++ map (indent (level + 2)) subInits
                ++ subDefs
       in (is ++ [initLine], ds ++ [classDef], ns ++ nestedFromSub)
generateApiStructure _ (FuncNode _) = ([], [], [])

generateApiClass :: [Action.ConvexFunction] -> (String, [String])
generateApiClass funcs =
  let tree = buildPathTree funcs
      (initLines, definitionLines, nestedModels) = generateApiStructure 1 tree
      header =
        [ "\n# --- API Client Class ---\n",
          "class API:",
          indent 1 "\"\"\"A type-safe client for your Convex API.\"\"\"",
          indent 1 "def __init__(self, client: ConvexClient):",
          indent 2 "self._client = client"
        ]
      body = map (indent 2) initLines ++ definitionLines
   in (unlines (header ++ body), nub nestedModels)

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
  Schema.VBytes -> ("bytes", False, False, [])
  Schema.VInt64 -> ("int", False, False, [])
  Schema.VFloat64 -> ("float", False, False, [])
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
