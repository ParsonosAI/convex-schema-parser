{-# LANGUAGE OverloadedStrings #-}

module Backend.Python (generatePythonCode) where

import qualified Convex.Action.Parser as Action
import qualified Convex.Parser as P
import qualified Convex.Schema.Parser as Schema
import Data.Char (isUpper, toLower, toUpper)
import Data.List (intercalate, isPrefixOf, nub, partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import PathTree

-- | Represents a generated Python definition (e.g., a class or a constant).
data Definition
  = Definition
  { defName :: String,
    defDeps :: Set.Set String,
    defCode :: String
  }
  deriving (Show, Eq, Ord)

-- | Sorts definitions topologically based on their dependencies.
--   A simple implementation that iteratively finds definitions with no remaining dependencies.
topologicalSort :: [Definition] -> [Definition]
topologicalSort defs = go defs [] (Set.fromList $ map defName defs)
  where
    go [] sorted _ = sorted
    go remaining sorted definedNames =
      let (ready, pending) = partition (\d -> Set.null (defDeps d `Set.intersection` definedNames)) remaining
       in if null ready && not (null pending)
            then error ("Circular dependency detected in definitions: " ++ show (map defName pending))
            else
              let newSorted = sorted ++ ready
                  newDefinedNames = definedNames `Set.difference` (Set.fromList $ map defName ready)
               in go pending newSorted newDefinedNames

-- Helper function to prepend a given number of spaces (4 per level).
indent :: Int -> String -> String
indent n s = replicate (n * 4) ' ' ++ s

generatePythonCode :: P.ParsedProject -> String
generatePythonCode project =
  let constantDefs = generateAllConstants (P.ppConstants project)
      tableDefs = generateAllTables (P.ppSchema project)
      (apiDef, apiNestedDefs) = generateApiClass (P.ppFunctions project)
      allDefs = nub $ constantDefs ++ tableDefs ++ apiNestedDefs ++ [apiDef]
      sortedDefs = topologicalSort allDefs
      definitionsCode = unlines $ map defCode sortedDefs
      aliasesCode = generateAliases (P.ppSchema project)
   in unlines
        [ generateHeader,
          definitionsCode,
          aliasesCode
        ]

-- | Generates the static header for the Python file.
generateHeader :: String
generateHeader =
  unlines
    [ "from typing import Any, Generic, Iterator, Literal, TypeVar",
      "",
      "from convex import ConvexClient, ConvexInt64",
      "from pydantic import BaseModel, Field, TypeAdapter, ValidationError",
      "from pydantic_core import core_schema",
      "",
      "",
      "class PydanticConvexInt64(ConvexInt64):",
      "    @classmethod",
      "    def __get_pydantic_core_schema__(cls, s, h) -> core_schema.CoreSchema:",
      "        from_int_schema = core_schema.no_info_after_validator_function(cls, core_schema.int_schema())",
      "",
      "        def validate_from_instance(v):",
      "            return PydanticConvexInt64(v.value)",
      "",
      "        from_instance_schema = core_schema.no_info_after_validator_function(",
      "            validate_from_instance, core_schema.is_instance_schema(ConvexInt64)",
      "        )",
      "",
      "        return core_schema.union_schema([from_instance_schema, from_int_schema])",
      "",
      "    def to_convex(self) -> ConvexInt64:",
      "        return ConvexInt64(self.value)",
      "",
      "T = TypeVar('T')",
      "class Id(str, Generic[T]):",
      "    @classmethod",
      "    def __get_pydantic_core_schema__(cls, s, h) -> core_schema.CoreSchema:",
      "        return core_schema.no_info_after_validator_function(cls, core_schema.str_schema())",
      ""
    ]

-- | Generates Python type aliases for all the named constants.
generateAllConstants :: Map.Map String Schema.ConvexType -> [Definition]
generateAllConstants constants =
  concatMap (generateConstant . fst) (Map.toList constants)
  where
    generateConstant :: String -> [Definition]
    generateConstant name =
      let constType = constants Map.! name
          (pyType, _, _, nestedDefs, deps) = toPythonTypeParts name constType
          code = name ++ " = " ++ pyType
          definition = Definition {defName = name, defDeps = deps, defCode = code}
       in definition : nestedDefs

-- | Generates Pydantic BaseModel classes for all tables.
generateAllTables :: Schema.Schema -> [Definition]
generateAllTables (Schema.Schema tables) =
  let (tableDefs, nestedDefs) = unzip $ map generateTable tables
   in tableDefs ++ concat nestedDefs

-- | Generates a single Pydantic BaseModel class for a table.
generateTable :: Schema.Table -> (Definition, [Definition])
generateTable table =
  let className = toClassName (Schema.tableName table)
      idField = Schema.Field "_id" (Schema.VId (Schema.tableName table))
      creationTimeField = Schema.Field "_creationTime" Schema.VNumber
      allFields = [idField, creationTimeField] ++ Schema.tableFields table
      (fieldLines, nestedDefsFromFields, fieldDeps) = unzip3 $ map (generateField className) allFields
      tableCode =
        unlines
          [ "class " ++ className ++ "(BaseModel):",
            unlines fieldLines,
            "",
            indent 1 "class Config:",
            indent 2 "populate_by_name: bool = True",
            unlines $ pythonToConvex 1 $ map (\f -> (Schema.fieldName f, Schema.fieldType f)) allFields
          ]
      deps = Set.delete className (Set.unions fieldDeps)
      definition = Definition {defName = className, defDeps = deps, defCode = tableCode}
   in (definition, concat nestedDefsFromFields)

pythonToConvex :: Int -> [(String, Schema.ConvexType)] -> [String]
pythonToConvex baseIndent fields =
  [ indent baseIndent $ "def to_convex(self) -> dict[str, Any]:",
    indent (baseIndent + 1) "return {",
    unlines $ map (indent (baseIndent + 2) . append ',' . fieldToConvexMap) fields,
    indent (baseIndent + 1) "}"
  ]
  where
    append :: Char -> String -> String
    append c s = s ++ [c]
    fieldToConvexMap :: (String, Schema.ConvexType) -> String
    fieldToConvexMap (fname, ctype) = "\"" ++ fname ++ "\" :" ++ fieldConversion
      where
        fieldConversion = case ctype of
          Schema.VInt64 -> "self." ++ fieldNameSnake ++ ".to_convex()"
          _ -> "self." ++ fieldNameSnake
        isSystemField = "_" `isPrefixOf` fname
        fieldNameSnake = if isSystemField then toSnakeCase (tail fname) else toSnakeCase fname

-- | Generates singular type aliases for all table documents.
generateAliases :: Schema.Schema -> String
generateAliases (Schema.Schema tables) =
  let header = "\n# --- Singular Type Aliases for Ergonomics ---\n"
   in header ++ (unlines $ map toAlias tables)
  where
    toAlias t = toSingular (Schema.tableName t) ++ " = " ++ toClassName (Schema.tableName t)

-- | Generates the code for a single Python function wrapper.
generateFunction :: Int -> Action.ConvexFunction -> (String, [Definition], Set.Set String)
generateFunction level func =
  let funcName = Action.funcName func
      (argSignature, payloadMapping, defsFromArgs, depsFromArgs) = generateArgSignature funcName (Action.funcArgs func)
      funcNameSnake = toSnakeCase funcName
      (rawReturnHint, isModelReturn, defsFromReturn, depsFromReturn) = getReturnType funcName (Action.funcReturn func)

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
   in (funcCode, defsFromArgs ++ defsFromReturn, depsFromArgs `Set.union` depsFromReturn)

generateSubscriptionFunction :: Int -> Action.ConvexFunction -> (String, [Definition], Set.Set String)
generateSubscriptionFunction level func =
  let funcName = Action.funcName func
      (argSignature, payloadMapping, defsFromArgs, depsFromArgs) = generateArgSignature funcName (Action.funcArgs func)
      funcNameSnake = "subscribe_" ++ toSnakeCase funcName
      (returnHint, _, defsFromReturn, depsFromReturn) = getReturnType funcName (Action.funcReturn func)
      finalReturnHint = "Iterator[" ++ returnHint ++ "]"
      fullFuncPath = "\"" ++ Action.funcPath func ++ ":" ++ funcName ++ "\""

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
   in (funcCode, defsFromArgs ++ defsFromReturn, depsFromArgs `Set.union` depsFromReturn)

generateApiStructure :: Int -> PathTree -> ([String], [String], [Definition], Set.Set String)
generateApiStructure level (DirNode dir) =
  let (inits, defs, nestedDefs, deps) = foldl processEntry ([], [], [], Set.empty) (Map.toList dir)
   in (inits, defs, nestedDefs, deps)
  where
    processEntry (is, ds, nds, ds_deps) (_name, FuncNode func) =
      let (funcDef, defsFromFunc, depsFromFunc) = generateFunction level func
          (subDef, defsFromSub, depsFromSub) =
            if Action.funcType func == Action.Query
              then generateSubscriptionFunction level func
              else ("", [], Set.empty)
       in (is, ds ++ [funcDef, subDef], nds ++ defsFromFunc ++ defsFromSub, ds_deps `Set.union` depsFromFunc `Set.union` depsFromSub)
    processEntry (is, ds, nds, ds_deps) (name, DirNode subDir) =
      let className = capitalize name
          attrName = toSnakeCase name
          initLine = "self." ++ attrName ++ " = self." ++ className ++ "(self._client)"
          (subInits, subDefs, defsFromSub, depsFromSub) = generateApiStructure (level + 1) (DirNode subDir)
          classDef =
            unlines $
              [ "",
                indent level ("class " ++ className ++ ":"),
                indent (level + 1) "def __init__(self, client: ConvexClient):",
                indent (level + 2) "self._client = client"
              ]
                ++ map (indent (level + 2)) subInits
                ++ subDefs
       in (is ++ [initLine], ds ++ [classDef], nds ++ defsFromSub, ds_deps `Set.union` depsFromSub)
generateApiStructure _ (FuncNode _) = ([], [], [], Set.empty)

generateApiClass :: [Action.ConvexFunction] -> (Definition, [Definition])
generateApiClass funcs =
  let tree = buildPathTree funcs
      (initLines, definitionLines, nestedDefs, deps) = generateApiStructure 1 tree
      header =
        [ "\n# --- API Client Class ---\n",
          "class API:",
          indent 1 "\"\"\"A type-safe client for your Convex API.\"\"\"",
          indent 1 "def __init__(self, client: ConvexClient):",
          indent 2 "self._client = client"
        ]
      body = map (indent 2) initLines ++ definitionLines
      apiCode = unlines (header ++ body)
      apiDef = Definition {defName = "API", defDeps = deps, defCode = apiCode}
   in (apiDef, nestedDefs)

-- Helper to generate Python function arguments and the payload dictionary mapping.
generateArgSignature :: String -> [(String, Schema.ConvexType)] -> (String, String, [Definition], Set.Set String)
generateArgSignature funcName args =
  let results = map (\(n, t) -> (n, toPythonTypeParts (capitalize funcName ++ capitalize n) t)) args
      sigParts = map (\(n, (t, _, _, _, _)) -> toSnakeCase n ++ ": " ++ t) results
      payloadParts = map (\(n, _) -> "\"" ++ n ++ "\": " ++ toSnakeCase n) results
      nestedDefs = concatMap (\(_, (_, _, _, defs, _)) -> defs) results
      deps = Set.unions $ map (\(_, (_, _, _, _, d)) -> d) results
      argSignature = intercalate ", " sigParts
   in (if length sigParts == 0 then argSignature else "*, " ++ argSignature, intercalate ", " payloadParts, nestedDefs, deps)

-- Helper to get the return type information.
getReturnType :: String -> Schema.ConvexType -> (String, Bool, [Definition], Set.Set String)
getReturnType funcName rt =
  let (pyType, _, _, nestedDefs, deps) = toPythonTypeParts (capitalize funcName ++ "Return") rt
      isModel = case rt of
        Schema.VObject _ -> True
        Schema.VArray (Schema.VObject _) -> True
        Schema.VReference _ -> True
        Schema.VArray (Schema.VReference _) -> True
        _ -> False
   in (pyType, isModel, nestedDefs, deps)

-- Helper to generate a single field line for a Pydantic model.
generateField :: String -> Schema.Field -> (String, [Definition], Set.Set String)
generateField parentClassName field =
  let originalFieldName = Schema.fieldName field
      isSystemField = "_" `isPrefixOf` originalFieldName
      fieldNameSnake = if isSystemField then toSnakeCase (tail originalFieldName) else toSnakeCase originalFieldName
      (pyType, isOpt, isArr, nested, deps) = toPythonTypeParts (parentClassName ++ capitalize originalFieldName) (Schema.fieldType field)

      fieldArgs =
        let defaultArg =
              if isOpt
                then if isArr then "default_factory=list" else "default=None"
                else "..."
            aliasArg = if isSystemField then Just ("alias=\"" ++ originalFieldName ++ "\"") else Nothing
         in intercalate ", " (catMaybes [Just defaultArg, aliasArg])

      fieldDef = fieldNameSnake ++ ": " ++ pyType ++ " = Field(" ++ fieldArgs ++ ")"
   in (indent 1 fieldDef, nested, deps)

-- Core recursive function to generate Python types from the AST.
toPythonTypeParts :: String -> Schema.ConvexType -> (String, Bool, Bool, [Definition], Set.Set String)
toPythonTypeParts nameHint typ = case typ of
  Schema.VString -> ("str", False, False, [], Set.empty)
  Schema.VNumber -> ("float", False, False, [], Set.empty)
  Schema.VInt64 -> ("PydanticConvexInt64", False, False, [], Set.empty)
  Schema.VFloat64 -> ("float", False, False, [], Set.empty)
  Schema.VBoolean -> ("bool", False, False, [], Set.empty)
  Schema.VBytes -> ("bytes", False, False, [], Set.empty)
  Schema.VAny -> ("Any", False, False, [], Set.empty)
  Schema.VNull -> ("None", True, False, [], Set.empty)
  Schema.VId t -> ("Id['" ++ toClassName t ++ "']", False, False, [], Set.singleton (toClassName t))
  Schema.VArray inner ->
    let (innerType, isOpt, isArr, nested, deps) = toPythonTypeParts nameHint inner
     in ("list[" ++ innerType ++ "]", isOpt, isArr, nested, deps)
  Schema.VOptional inner ->
    let (innerType, _, innerIsArray, nested, deps) = toPythonTypeParts nameHint inner
     in (innerType ++ " | None", True, innerIsArray, nested, deps)
  Schema.VUnion types ->
    let results = map (toPythonTypeParts nameHint) types
        pyTypes = nub $ map (\(t, _, _, _, _) -> t) results
        nested = concatMap (\(_, _, _, d, _) -> d) results
        deps = Set.unions $ map (\(_, _, _, _, d) -> d) results
     in (intercalate " | " pyTypes, False, False, nested, deps)
  Schema.VLiteral s -> ("Literal[\"" ++ s ++ "\"]", False, False, [], Set.empty)
  Schema.VReference n -> (n, False, False, [], Set.singleton n)
  Schema.VObject fields ->
    let className = capitalize nameHint ++ "Object"
        (fieldLines, nestedDefs, fieldDeps) = unzip3 $ map (generateField className) (map (\(n, t) -> Schema.Field n t) fields)
        newModelCode =
          unlines $
            [ "class " ++ className ++ "(BaseModel):",
              unlines fieldLines,
              "",
              indent 1 "class Config:",
              indent 2 "populate_by_name: bool = True",
              "",
              unlines $ pythonToConvex 1 fields
            ]
        deps = Set.unions fieldDeps
        newModelDef = Definition {defName = className, defDeps = deps, defCode = newModelCode}
     in (className, False, False, newModelDef : concat nestedDefs, Set.singleton className)
  Schema.VVoid -> ("None", True, False, [], Set.empty)

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
