{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Convex.Parser (parseProject, parseProjectFromContents, ParsedProject (..), apiFileParser, runUnificationPass) where

import Control.Monad (forM, void)
import qualified Convex.Action.Parser as Action
import qualified Convex.Schema.Parser as Schema
import Data.Char (toUpper)
import Data.List (sort, sortOn)
import qualified Data.Map as Map
import System.FilePath (replaceExtension, (</>))
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

data ParsedProject = ParsedProject
  { ppSchema :: Schema.Schema,
    ppConstants :: Map.Map String Schema.ConvexType,
    ppFunctions :: [Action.ConvexFunction]
  }
  deriving (Show, Eq)

apiLexer :: Token.TokenParser ()
apiLexer =
  Token.makeTokenParser
    emptyDef
      { Token.identStart = letter <|> char '_',
        Token.identLetter = alphaNum <|> char '_',
        Token.reservedNames = ["typeof", "declare", "const", "ApiFromModules"]
      }

apiStringLiteral :: Parsec String () String
apiStringLiteral = Token.stringLiteral apiLexer

apiIdentifier :: Parsec String () String
apiIdentifier = Token.identifier apiLexer

apiReserved :: String -> Parsec String () ()
apiReserved = Token.reserved apiLexer

apiFileParser :: Parsec String () [String] -- We only need the paths
apiFileParser = do
  -- Find the start of the object we care about
  void $ manyTill anyChar (try (string "declare const fullApi: ApiFromModules<"))
  -- Enter the braces of the object
  paths <- Token.braces apiLexer (sepEndBy singlePath (Token.lexeme apiLexer (char ';')))
  return paths
  where
    -- This parses a single line like: "admin/actions": typeof admin_actions
    singlePath = do
      path <- apiStringLiteral <|> apiIdentifier
      void $ Token.lexeme apiLexer $ char ':'
      void $ apiReserved "typeof"
      void $ apiIdentifier -- Consume the alias, we don't need it
      return path

parseProjectFromContents ::
  String ->
  String ->
  [(String, String)] ->
  IO (Either String ParsedProject)
parseProjectFromContents schemaContent apiFileContent actionContents = do
  -- 1. Parse Schema
  schemaResult <- Schema.parseSchema schemaContent
  case schemaResult of
    Left err -> return $ Left ("Failed to parse schema.ts: " ++ show err)
    Right schemaFile -> do
      let initialState = Schema.ParserState {Schema.psConstants = Schema.parsedConstants schemaFile}

      -- Parse API file to get module paths
      let modulePaths = case parse apiFileParser "(api.d.ts)" apiFileContent of
            Left _ -> [] -- Or should this be an error? The original returns []
            Right paths -> filter (/= "schema") paths

      let actionContentMap = Map.fromList actionContents

      -- Parse action files
      moduleResults <- forM modulePaths $ \modulePath -> do
        let astPath = replaceExtension modulePath ""
        case Map.lookup modulePath actionContentMap of
          Nothing -> do
            putStrLn $ "Action content not found for module: " ++ modulePath
            return ([], [])
          Just actionContent -> do
            actionResult <- runParserT (Action.parseActionFile astPath) initialState modulePath actionContent
            case actionResult of
              Left err -> do
                putStrLn $ "Failed to parse actions from: " ++ modulePath ++ " | Error: " ++ show err
                return ([], [])
              Right (funcs, extraTypes) -> do
                putStrLn $ "Parsed actions from: " ++ modulePath
                putStrLn $ show (length funcs) ++ " functions found"
                putStrLn $ show (length extraTypes) ++ " extra types found"
                return (funcs, extraTypes)

      let (allFunctions, allExtraTypes) = foldr (\(fs, ts) (accF, accT) -> (fs ++ accF, ts ++ accT)) ([], []) moduleResults

      -- Construct and unify project
      let project =
            ParsedProject
              { ppSchema = Schema.parsedSchema schemaFile,
                ppConstants =
                  Schema.parsedConstants schemaFile
                    `Map.union` (Map.fromList [(Action.dtsTypeName t, Schema.VObject . Action.dtsTypeFields $ t) | t <- allExtraTypes]),
                ppFunctions = allFunctions
              }

      return . Right . runUnificationPass $ project

parseProject :: FilePath -> FilePath -> IO (Either String ParsedProject)
parseProject schemaPath declRootDir = do
  -- Parse the source schema file first to get tables and the initial state with constants.
  schemaContent <- readFile schemaPath
  -- Parse the _generated/api.d.ts file to discover function modules.
  let apiFilePath = declRootDir </> "_generated" </> "api.d.ts"
  apiFileContent <- readFile apiFilePath
  let modulePaths = case parse apiFileParser apiFilePath apiFileContent of
        Left _ -> []
        Right paths -> filter (/= "schema") paths

  putStrLn $ "Found " ++ show (length modulePaths) ++ " action modules in: " ++ apiFilePath

  -- For each discovered module, parse its corresponding .d.ts file.
  actionContents <- forM modulePaths $ \modulePath -> do
    let fullPath = declRootDir </> replaceExtension modulePath ".d.ts"
    content <- readFile fullPath
    return (modulePath, content)

  parseProjectFromContents schemaContent apiFileContent actionContents

type UnionSignatureMap = Map.Map [String] String

type ObjectSignatureMap = Map.Map [(String, Schema.ConvexType)] String

-- | Pre-processes the parsed project to replace anonymous unions and objects
-- with named references if they structurally match. This is done iteratively
-- to a fixed point to handle nested structures.
runUnificationPass :: ParsedProject -> ParsedProject
runUnificationPass project =
  let ephemeralProject = addTableDocsToConstants project
      unifiedProject = go ephemeralProject
   in unifiedProject {ppConstants = ppConstants project} -- Discard ephemeral constants
  where
    go currentProject =
      let nextProject = unifyOnce currentProject
       in if nextProject == currentProject
            then currentProject
            else go nextProject

canonicalizeType :: Schema.ConvexType -> Schema.ConvexType
canonicalizeType (Schema.VObject fields) =
  Schema.VObject . sortOn fst . map (\(n, t) -> (n, canonicalizeType t)) $ fields
canonicalizeType (Schema.VUnion types) =
  Schema.VUnion . sort . map canonicalizeType $ types
canonicalizeType (Schema.VArray t) = Schema.VArray (canonicalizeType t)
canonicalizeType (Schema.VOptional t) = Schema.VOptional (canonicalizeType t)
canonicalizeType other = other

addTableDocsToConstants :: ParsedProject -> ParsedProject
addTableDocsToConstants project =
  let tableDocs =
        Map.fromList
          [ ( toPascalCase (Schema.tableName table) ++ "Doc",
              Schema.VObject . sortOn fst $
                [("_id", Schema.VId (Schema.tableName table)), ("_creationTime", Schema.VNumber)]
                  ++ map
                    (\f -> (Schema.fieldName f, Schema.fieldType f))
                    (Schema.tableFields table)
            )
          | table <- Schema.getTables (ppSchema project)
          ]
      allConstants = Map.union (ppConstants project) tableDocs
   in project {ppConstants = allConstants}

toPascalCase :: String -> String
toPascalCase [] = []
toPascalCase (h : t) = toUpper h : t

buildUnionSignatureMap :: Map.Map String Schema.ConvexType -> UnionSignatureMap
buildUnionSignatureMap constants =
  Map.fromList
    [ (sort $ map Schema.getLiteralString literals, name)
    | (name, Schema.VUnion literals) <- Map.toList constants,
      all Schema.isLiteral literals
    ]

buildObjectSignatureMap :: Map.Map String Schema.ConvexType -> ObjectSignatureMap
buildObjectSignatureMap constants =
  Map.fromList
    [ (sortOn fst fields, name)
    | (name, Schema.VObject fields) <- Map.toList constants
    ]

unifyFunctionTypes :: (Schema.ConvexType -> Schema.ConvexType) -> Action.ConvexFunction -> Action.ConvexFunction
unifyFunctionTypes unifyType func =
  func
    { Action.funcArgs = map (\(name, t) -> (name, unifyType t)) (Action.funcArgs func),
      Action.funcReturn = unifyType (Action.funcReturn func)
    }

unifyTypeRecursively :: Maybe String -> UnionSignatureMap -> ObjectSignatureMap -> Schema.ConvexType -> Schema.ConvexType
unifyTypeRecursively mCurrentName unionMap objectMap = go
  where
    goRec = unifyTypeRecursively Nothing unionMap objectMap

    go u@(Schema.VUnion literals)
      | all Schema.isLiteral literals =
          let signature = sort $ map Schema.getLiteralString literals
           in case Map.lookup signature unionMap of
                Just refName ->
                  if Just refName == mCurrentName
                    then canonicalizeType u
                    else Schema.VReference refName
                Nothing -> canonicalizeType u
      | otherwise =
          let unifiedUnion = Schema.VUnion (map goRec literals)
           in canonicalizeType unifiedUnion
    go (Schema.VObject fields) =
      let unifiedFields = map (\(name, t) -> (name, goRec t)) fields
          canonicalAttempt = canonicalizeType (Schema.VObject unifiedFields)
       in case canonicalAttempt of
            Schema.VObject signature ->
              let canonicalObject = Schema.VObject signature
               in case Map.lookup signature objectMap of
                    Just refName ->
                      if Just refName == mCurrentName
                        then canonicalObject
                        else Schema.VReference refName
                    Nothing -> canonicalObject
            _ -> canonicalAttempt -- Should not happen, but safer
    go (Schema.VArray inner) = Schema.VArray (goRec inner)
    go (Schema.VOptional inner) = Schema.VOptional (goRec inner)
    go otherType = otherType

unifyOnce :: ParsedProject -> ParsedProject
unifyOnce project =
  let unionMap = buildUnionSignatureMap (ppConstants project)
      initialObjectMap = buildObjectSignatureMap (ppConstants project)

      -- First, unify the constants themselves. This resolves nested anonymous objects
      -- within the constants first, creating a canonical representation for this pass.
      unifiedConstants =
        Map.mapWithKey (\k -> unifyTypeRecursively (Just k) unionMap initialObjectMap) (ppConstants project)

      -- Now, build the object map for function unification from these *newly unified* constants.
      -- This map contains the canonical object structures for this pass.
      finalObjectMap = buildObjectSignatureMap unifiedConstants

      -- Create the unification function for anonymous types found in function signatures.
      unifyAnonType = unifyTypeRecursively Nothing unionMap finalObjectMap

      unifiedFunctions = map (unifyFunctionTypes unifyAnonType) (ppFunctions project)
   in project
        { ppConstants = unifiedConstants,
          ppFunctions = unifiedFunctions
        }
