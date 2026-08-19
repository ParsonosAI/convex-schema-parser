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
      tableDefs = generateAllTables (P.ppConstants project) (P.ppSchema project)
      (apiDef, apiNestedDefs) = generateApiClass (P.ppConstants project) (P.ppFunctions project)
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
      "def _to_convex(value: Any) -> Any:",
      "    converter = getattr(value, 'to_convex', None)",
      "    return converter() if converter is not None else value",
      "",
      "class UnsetType:",
      "    __slots__ = ()",
      "",
      "UNSET = UnsetType()",
      "",
      "T = TypeVar('T')",
      "class Id(str, Generic[T]):",
      "    @classmethod",
      "    def __get_pydantic_core_schema__(cls, s, h) -> core_schema.CoreSchema:",
      "        return core_schema.no_info_after_validator_function(cls, core_schema.str_schema())",
      ""
    ]

toPayloadExpr :: String -> Schema.ConvexType -> String
toPayloadExpr varName typ = case typ of
  Schema.VString -> varName
  Schema.VNumber -> varName
  Schema.VInt64 -> varName ++ ".to_convex()"
  Schema.VFloat64 -> varName
  Schema.VBoolean -> varName
  Schema.VBytes -> varName
  Schema.VAny -> varName
  Schema.VNull -> varName
  Schema.VId _ -> varName
  Schema.VLiteral _ -> varName
  Schema.VVoid -> varName
  Schema.VReference _ -> "_to_convex(" ++ varName ++ ")"
  Schema.VObject _ -> varName ++ ".to_convex()"
  Schema.VArray inner ->
    "[" ++ toPayloadExpr "item" inner ++ " for item in " ++ varName ++ "]"
  Schema.VOptional inner ->
    toPayloadExpr varName inner
  Schema.VUnion _ ->
    "_to_convex(" ++ varName ++ ")"

acceptsNull :: Map.Map String Schema.ConvexType -> Schema.ConvexType -> Bool
acceptsNull constants = go Set.empty
  where
    go _ Schema.VNull = True
    go _ Schema.VAny = True
    go seen (Schema.VReference name)
      | name `Set.member` seen = False
      | otherwise = maybe False (go (Set.insert name seen)) (Map.lookup name constants)
    go seen (Schema.VUnion types) = any (go seen) types
    go seen (Schema.VOptional inner) = go seen inner
    go _ _ = False

-- | Generates Python type aliases for all the named constants.
generateAllConstants :: Map.Map String Schema.ConvexType -> [Definition]
generateAllConstants constants =
  concatMap (generateConstant . fst) (Map.toList constants)
  where
    generateConstant :: String -> [Definition]
    generateConstant name =
      let constType = constants Map.! name
          (pyType, _, _, nestedDefs, deps) = toPythonTypeParts constants name constType
          code = name ++ " = " ++ pyType
          definition = Definition {defName = name, defDeps = deps, defCode = code}
       in definition : nestedDefs

-- | Generates Pydantic BaseModel classes for all tables.
generateAllTables :: Map.Map String Schema.ConvexType -> Schema.Schema -> [Definition]
generateAllTables constants (Schema.Schema tables) =
  let (tableDefs, nestedDefs) = unzip $ map (generateTable constants) tables
   in tableDefs ++ concat nestedDefs

-- | Generates a single Pydantic BaseModel class for a table.
generateTable :: Map.Map String Schema.ConvexType -> Schema.Table -> (Definition, [Definition])
generateTable constants table =
  let className = toClassName (Schema.tableName table)
      idField = Schema.Field "_id" (Schema.VId (Schema.tableName table))
      creationTimeField = Schema.Field "_creationTime" Schema.VNumber
      allFields = [idField, creationTimeField] ++ Schema.tableFields table
      (fieldLines, nestedDefsFromFields, fieldDeps) = unzip3 $ map (generateField constants className) allFields
      tableCode =
        unlines
          [ "class " ++ className ++ "(BaseModel):",
            unlines fieldLines,
            "",
            indent 1 "class Config:",
            indent 2 "populate_by_name: bool = True",
            unlines $ pythonToConvex constants 1 $ map (\f -> (Schema.fieldName f, Schema.fieldType f)) allFields
          ]
      deps = Set.delete className (Set.unions fieldDeps)
      definition = Definition {defName = className, defDeps = deps, defCode = tableCode}
   in (definition, concat nestedDefsFromFields)

pythonToConvex :: Map.Map String Schema.ConvexType -> Int -> [(String, Schema.ConvexType)] -> [String]
pythonToConvex constants baseIndent fields =
  [ indent baseIndent "def to_convex(self) -> dict[str, Any]:",
    indent (baseIndent + 1) "payload: dict[str, Any] = {}"
  ]
    ++ concatMap fieldToConvexLines fields
    ++ [indent (baseIndent + 1) "return payload"]
  where
    fieldToConvexLines :: (String, Schema.ConvexType) -> [String]
    fieldToConvexLines (fname, ctype) =
      case ctype of
        Schema.VOptional inner ->
          let assignment = payloadAssignment fname (toPayloadExpr fieldAccess inner)
           in [ indent (baseIndent + 1) ("if " ++ optionalCondition fieldNameSnake fieldAccess inner ++ ":"),
                indent (baseIndent + 2) assignment
              ]
        _ ->
          [indent (baseIndent + 1) (payloadAssignment fname (toPayloadExpr fieldAccess ctype))]
      where
        isSystemField = "_" `isPrefixOf` fname
        fieldNameSnake = if isSystemField then toSnakeCase (drop 1 fname) else toSnakeCase fname
        fieldAccess = "self." ++ fieldNameSnake

    optionalCondition :: String -> String -> Schema.ConvexType -> String
    optionalCondition fieldName fieldAccess inner
      | acceptsNull constants inner =
          fieldAccess ++ " is not None or \"" ++ fieldName ++ "\" in self.model_fields_set"
      | otherwise = fieldAccess ++ " is not None"

    payloadAssignment :: String -> String -> String
    payloadAssignment fieldName valueExpr =
      "payload[\"" ++ fieldName ++ "\"] = " ++ valueExpr

-- | Generates singular type aliases for all table documents.
generateAliases :: Schema.Schema -> String
generateAliases (Schema.Schema tables) =
  let header = "\n# --- Singular Type Aliases for Ergonomics ---\n"
   in header ++ (unlines $ map toAlias tables)
  where
    toAlias t = toSingular (Schema.tableName t) ++ " = " ++ toClassName (Schema.tableName t)

-- | Generates the code for a single Python function wrapper.
generateFunction :: Map.Map String Schema.ConvexType -> Int -> Action.ConvexFunction -> (String, [Definition], Set.Set String)
generateFunction constants level func =
  let funcName = Action.funcName func
      (argSignature, payloadLines, defsFromArgs, depsFromArgs) = generateArgSignature constants funcName (Action.funcArgs func)
      funcNameSnake = toSnakeCase funcName
      (rawReturnHint, isModelReturn, defsFromReturn, depsFromReturn) = getReturnType constants funcName (Action.funcReturn func)

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
                  then "TypeAdapter(" ++ rawReturnHint ++ ").validate_python(raw_result)"
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
            indent (level + 1) "payload: dict[str, Any] = {}",
            unlines $ map (indent (level + 1)) payloadLines,
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

generateSubscriptionFunction :: Map.Map String Schema.ConvexType -> Int -> Action.ConvexFunction -> (String, [Definition], Set.Set String)
generateSubscriptionFunction constants level func =
  let funcName = Action.funcName func
      (argSignature, payloadLines, defsFromArgs, depsFromArgs) = generateArgSignature constants funcName (Action.funcArgs func)
      funcNameSnake = "subscribe_" ++ toSnakeCase funcName
      (returnHint, _, defsFromReturn, depsFromReturn) = getReturnType constants funcName (Action.funcReturn func)
      finalReturnHint = "Iterator[" ++ returnHint ++ "]"
      fullFuncPath = "\"" ++ Action.funcPath func ++ ":" ++ funcName ++ "\""

      adapterCreation = indent (level + 1) ("adapter = TypeAdapter(" ++ returnHint ++ ")")
      validationLogic = indent (level + 3) "validated_result = adapter.validate_python(raw_result)"

      funcCode =
        unlines
          [ indent level ("def " ++ funcNameSnake ++ "(self, " ++ argSignature ++ ") -> " ++ finalReturnHint ++ ":"),
            indent (level + 1) ("\"\"\"Subscribes to the " ++ fullFuncPath ++ " query.\"\"\""),
            indent (level + 1) "payload: dict[str, Any] = {}",
            unlines $ map (indent (level + 1)) payloadLines,
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

generateApiStructure :: Map.Map String Schema.ConvexType -> Int -> PathTree -> ([String], [String], [Definition], Set.Set String)
generateApiStructure constants level (DirNode dir) =
  let (inits, defs, nestedDefs, deps) = foldl processEntry ([], [], [], Set.empty) (Map.toList dir)
   in (inits, defs, nestedDefs, deps)
  where
    processEntry (is, ds, nds, ds_deps) (_name, FuncNode func) =
      let (funcDef, defsFromFunc, depsFromFunc) = generateFunction constants level func
          (subDef, defsFromSub, depsFromSub) =
            if Action.funcType func == Action.Query
              then generateSubscriptionFunction constants level func
              else ("", [], Set.empty)
       in (is, ds ++ [funcDef, subDef], nds ++ defsFromFunc ++ defsFromSub, ds_deps `Set.union` depsFromFunc `Set.union` depsFromSub)
    processEntry (is, ds, nds, ds_deps) (name, DirNode subDir) =
      let className = capitalize name
          attrName = toSnakeCase name
          initLine = "self." ++ attrName ++ " = self." ++ className ++ "(self._client)"
          (subInits, subDefs, defsFromSub, depsFromSub) = generateApiStructure constants (level + 1) (DirNode subDir)
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
generateApiStructure _ _ (FuncNode _) = ([], [], [], Set.empty)

generateApiClass :: Map.Map String Schema.ConvexType -> [Action.ConvexFunction] -> (Definition, [Definition])
generateApiClass constants funcs =
  let tree = buildPathTree funcs
      (initLines, definitionLines, nestedDefs, deps) = generateApiStructure constants 1 tree
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
generateArgSignature :: Map.Map String Schema.ConvexType -> String -> [(String, Schema.ConvexType)] -> (String, [String], [Definition], Set.Set String)
generateArgSignature constants funcName args =
  let results = map (\(n, t) -> (n, t, toPythonTypeParts constants (capitalize funcName ++ capitalize n) t)) args
      sigParts = map mkSignaturePart results
      payloadLines = concatMap mkPayloadLines results
      nestedDefs = concatMap (\(_, _, (_, _, _, defs, _)) -> defs) results
      deps = Set.unions $ map (\(_, _, (_, _, _, _, d)) -> d) results
      argSignature = intercalate ", " sigParts
   in ( if null sigParts then argSignature else "*, " ++ argSignature,
        payloadLines,
        nestedDefs,
        deps
      )
  where
    mkSignaturePart :: (String, Schema.ConvexType, (String, Bool, Bool, [Definition], Set.Set String)) -> String
    mkSignaturePart (name, convexType, (pyType, _, _, _, _)) =
      let base = toSnakeCase name ++ ": " ++ pyType
       in case convexType of
            Schema.VOptional inner
              | acceptsNull constants inner -> base ++ " | UnsetType = UNSET"
              | otherwise -> base ++ " = None"
            _ -> base

    mkPayloadLines :: (String, Schema.ConvexType, (String, Bool, Bool, [Definition], Set.Set String)) -> [String]
    mkPayloadLines (originalName, convexType, (_pyType, _isOpt, _isArr, _nested, _deps)) =
      let pyName = toSnakeCase originalName
          payloadKey = "\"" ++ originalName ++ "\""
          assignment valueType =
            "payload[" ++ payloadKey ++ "] = " ++ toPayloadExpr pyName valueType
       in case convexType of
            Schema.VOptional inner ->
              [ if acceptsNull constants inner
                  then "if not isinstance(" ++ pyName ++ ", UnsetType):"
                  else "if " ++ pyName ++ " is not None:",
                indent 1 $ assignment inner
              ]
            _ -> [assignment convexType]

-- Helper to get the return type information.
getReturnType :: Map.Map String Schema.ConvexType -> String -> Schema.ConvexType -> (String, Bool, [Definition], Set.Set String)
getReturnType constants funcName rt =
  let (pyType, _, _, nestedDefs, deps) = toPythonTypeParts constants (capitalize funcName ++ "Return") rt
      isModel = containsModel rt
   in (pyType, isModel, nestedDefs, deps)
  where
    containsModel convexType = case convexType of
      Schema.VObject _ -> True
      Schema.VReference _ -> True
      Schema.VArray inner -> containsModel inner
      Schema.VOptional inner -> containsModel inner
      Schema.VUnion unionTypes -> any containsModel unionTypes
      _ -> False

-- Helper to generate a single field line for a Pydantic model.
generateField :: Map.Map String Schema.ConvexType -> String -> Schema.Field -> (String, [Definition], Set.Set String)
generateField constants parentClassName field =
  let originalFieldName = Schema.fieldName field
      isSystemField = "_" `isPrefixOf` originalFieldName
      fieldNameSnake = if isSystemField then toSnakeCase (drop 1 originalFieldName) else toSnakeCase originalFieldName
      (pyType, isOpt, _isArr, nested, deps) = toPythonTypeParts constants (parentClassName ++ capitalize originalFieldName) (Schema.fieldType field)

      fieldArgs =
        let defaultArg =
              if isOpt
                then "default=None"
                else "..."
            aliasArg = if isSystemField then Just ("alias=\"" ++ originalFieldName ++ "\"") else Nothing
         in intercalate ", " (catMaybes [Just defaultArg, aliasArg])

      fieldDef = fieldNameSnake ++ ": " ++ pyType ++ " = Field(" ++ fieldArgs ++ ")"
   in (indent 1 fieldDef, nested, deps)

-- Core recursive function to generate Python types from the AST.
toPythonTypeParts :: Map.Map String Schema.ConvexType -> String -> Schema.ConvexType -> (String, Bool, Bool, [Definition], Set.Set String)
toPythonTypeParts constants nameHint typ = case typ of
  Schema.VString -> ("str", False, False, [], Set.empty)
  Schema.VNumber -> ("float", False, False, [], Set.empty)
  Schema.VInt64 -> ("PydanticConvexInt64", False, False, [], Set.empty)
  Schema.VFloat64 -> ("float", False, False, [], Set.empty)
  Schema.VBoolean -> ("bool", False, False, [], Set.empty)
  Schema.VBytes -> ("bytes", False, False, [], Set.empty)
  Schema.VAny -> ("Any", False, False, [], Set.empty)
  Schema.VNull -> ("None", False, False, [], Set.empty)
  -- Id[T] is a phantom typed string: the generated model does not embed or
  -- validate a T instance, so mutually referencing table IDs do not impose a
  -- class-definition ordering dependency.
  Schema.VId t -> ("Id['" ++ toClassName t ++ "']", False, False, [], Set.empty)
  Schema.VArray inner ->
    let (innerType, _, _, nested, deps) = toPythonTypeParts constants nameHint inner
     in ("list[" ++ innerType ++ "]", False, True, nested, deps)
  Schema.VOptional inner ->
    let (innerType, _, innerIsArray, nested, deps) = toPythonTypeParts constants nameHint inner
        optionalType =
          if acceptsNull constants inner
            then innerType
            else innerType ++ " | None"
     in (optionalType, True, innerIsArray, nested, deps)
  Schema.VUnion types ->
    let results = zipWith (\index unionType -> toPythonTypeParts constants (nameHint ++ "Variant" ++ show index) unionType) [1 :: Int ..] types
        pyTypes = nub $ map (\(t, _, _, _, _) -> t) results
        nested = concatMap (\(_, _, _, d, _) -> d) results
        deps = Set.unions $ map (\(_, _, _, _, d) -> d) results
     in (intercalate " | " pyTypes, False, False, nested, deps)
  Schema.VLiteral s -> ("Literal[\"" ++ s ++ "\"]", False, False, [], Set.empty)
  Schema.VReference n -> (n, False, False, [], Set.singleton n)
  Schema.VObject fields ->
    let className = capitalize nameHint ++ "Object"
        (fieldLines, nestedDefs, fieldDeps) = unzip3 $ map (generateField constants className) (map (\(n, t) -> Schema.Field n t) fields)
        newModelCode =
          unlines $
            [ "class " ++ className ++ "(BaseModel):",
              unlines fieldLines,
              "",
              indent 1 "class Config:",
              indent 2 "populate_by_name: bool = True",
              "",
              unlines $ pythonToConvex constants 1 fields
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
