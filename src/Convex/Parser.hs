{-# LANGUAGE OverloadedStrings #-}

module Convex.Parser (parseProject, ParsedProject (..), apiFileParser) where

import Control.Monad (forM, void)
import qualified Convex.Action.Parser as Action
import qualified Convex.Schema.Parser as Schema
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

parseProject :: FilePath -> FilePath -> IO (Either String ParsedProject)
parseProject schemaPath declRootDir = do
  -- Parse the source schema file first to get tables and the initial state with constants.
  schemaContent <- readFile schemaPath
  schemaResult <- Schema.parseSchema schemaContent

  case schemaResult of
    Left err -> return $ Left ("Failed to parse schema.ts: " ++ show err)
    Right schemaFile -> do
      -- Re-construct the initial state for the action parser from the parsed constants.
      let initialState = Schema.ParserState {Schema.psConstants = Schema.parsedConstants schemaFile}

      -- Parse the _generated/api.d.ts file to discover function modules.
      let apiFilePath = declRootDir </> "_generated" </> "api.d.ts"
      apiFileContent <- readFile apiFilePath
      let modulePaths = case parse apiFileParser apiFilePath apiFileContent of
            Left _ -> []
            Right paths -> filter (/= "schema") paths

      putStrLn $ "Found " ++ show (length modulePaths) ++ " action modules in: " ++ apiFilePath

      -- For each discovered module, parse its corresponding .d.ts file.
      allFunctions <- fmap concat $ forM modulePaths $ \modulePath -> do
        let fullPath = declRootDir </> replaceExtension modulePath ".d.ts"
        let astPath = replaceExtension modulePath ""

        actionContent <- readFile fullPath
        actionResult <- runParserT (Action.parseActionFile astPath) initialState fullPath actionContent

        case actionResult of
          Left err -> do
            putStrLn $ "Failed to parse actions from: " ++ fullPath ++ " | Error: " ++ show err
            return []
          Right funcs -> do
            putStrLn $ "Parsed actions from: " ++ fullPath
            putStrLn $ show (length funcs) ++ " functions found"
            return funcs

      let project =
            ParsedProject
              { ppSchema = Schema.parsedSchema schemaFile,
                ppConstants = Schema.parsedConstants schemaFile,
                ppFunctions = allFunctions
              }

      return $ Right project
