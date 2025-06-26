{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Convex.Schema.Parser
  ( parseSchema,
    ParsedFile (..),
    Schema (..),
    Table (..),
    Index (..),
    Field (..),
    ConvexType (..),
  )
where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec
import qualified Text.Parsec.Language as Token
import qualified Text.Parsec.Token as Token

type SchemaParser a = ParsecT String ParserState IO a

data ParserState = ParserState
  { -- Holds a map of names to their defined Convex types.
    psConstants :: Map String ConvexType
  }
  deriving (Show, Eq)

initialState :: ParserState
initialState = ParserState {psConstants = Map.empty}

data ParsedFile = ParsedFile
  { parsedConstants :: Map String ConvexType,
    parsedSchema :: Schema
  }
  deriving (Show, Eq)

newtype Schema = Schema {getTables :: [Table]}
  deriving (Show, Eq)

data Index = Index
  { indexName :: String,
    indexFields :: [String]
  }
  deriving (Show, Eq)

data Table = Table
  { tableName :: String,
    tableFields :: [Field],
    tableIndexes :: [Index]
  }
  deriving (Show, Eq)

data Field = Field
  { fieldName :: String,
    fieldType :: ConvexType
  }
  deriving (Show, Eq)

data ConvexType
  = VString -- e.g., v.string()
  | VNumber -- e.g., v.number()
  | VBoolean -- e.g., v.boolean()
  | VNull -- e.g., v.null()
  | VAny -- e.g., v.any()
  | VId String -- e.g., v.id("user_id")
  | VArray ConvexType -- e.g., v.array(v.string())
  | VObject [(String, ConvexType)] -- e.g., v.object({ name: v.string(), age: v.number() })
  | VOptional ConvexType -- e.g., v.optional(v.string())
  | VUnion [ConvexType] -- e.g., v.union(v.literal("a"), v.literal("b"))
  | VLiteral String -- e.g., v.literal("read")
  | VReference String -- e.g., a reference to `roleEnum`
  deriving (Show, Eq, Ord)

langDef :: Token.GenLanguageDef String ParserState IO
langDef =
  Token.LanguageDef
    { Token.commentStart = "/*",
      Token.nestedComments = True,
      Token.commentEnd = "*/",
      Token.commentLine = "//",
      Token.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
      Token.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      Token.reservedOpNames = [],
      Token.identStart = letter <|> char '_',
      Token.identLetter = alphaNum <|> char '_',
      Token.reservedNames =
        [ "defineSchema",
          "defineTable",
          "v",
          "export",
          "default",
          "import",
          "from",
          "const",
          "type",
          "keyof",
          "typeof"
        ],
      Token.caseSensitive = True
    }

lexer :: Token.GenTokenParser String ParserState IO
lexer = Token.makeTokenParser langDef

whiteSpace :: SchemaParser ()
whiteSpace = Token.whiteSpace lexer

lexeme :: SchemaParser a -> SchemaParser a
lexeme = Token.lexeme lexer

identifier :: SchemaParser String
identifier = Token.identifier lexer

stringLiteral :: SchemaParser String
stringLiteral = Token.stringLiteral lexer

reserved :: String -> SchemaParser ()
reserved = Token.reserved lexer

parens :: SchemaParser a -> SchemaParser a
parens = Token.parens lexer

braces :: SchemaParser a -> SchemaParser a
braces = Token.braces lexer

brackets :: SchemaParser a -> SchemaParser a
brackets = Token.brackets lexer

fieldToTuple :: Field -> (String, ConvexType)
fieldToTuple (Field name typ) = (name, typ)

fieldParser :: SchemaParser Field
fieldParser = lexeme $ do
  key <- identifier <|> stringLiteral
  void $ lexeme $ char ':'
  value <- convexTypeParser -- This now handles all possible types
  return $ Field key value

indexParser :: SchemaParser Index
indexParser = lexeme $ do
  void $ char '.'
  reserved "index"
  (iName, iFields) <- parens $ do
    name <- stringLiteral
    void $ lexeme $ char ','
    fields <- brackets $ sepEndBy stringLiteral (lexeme $ char ',')
    return (name, fields)
  return $ Index iName iFields

tableParser :: SchemaParser Table
tableParser = lexeme $ do
  tName <- identifier <|> stringLiteral
  void $ lexeme $ char ':'
  reserved "defineTable"
  fields <- parens $ braces $ sepEndBy fieldParser (lexeme $ char ',')
  -- After the table definition, parse zero or more chained .index() calls.
  indexes <- many indexParser
  return $ Table tName fields indexes

single :: SchemaParser a -> SchemaParser a
single p = p <* (optional (lexeme $ char ','))

convexTypeParser :: SchemaParser ConvexType
convexTypeParser =
  -- It's a choice between a `v.` call or a named reference.
  try vParser <|> referenceParser
  where
    -- Parses things like `v.string()`, `v.optional(...)`, etc.
    vParser = do
      void $ lexeme $ reserved "v"
      void $ lexeme $ char '.'
      typeName <- identifier
      case typeName of
        "string" -> VString <$ parens (return ())
        "number" -> VNumber <$ parens (return ())
        "boolean" -> VBoolean <$ parens (return ())
        "null" -> VNull <$ parens (return ())
        "any" -> VAny <$ parens (return ())
        "id" -> VId <$> parens stringLiteral
        "array" -> VArray <$> (single (parens (single convexTypeParser)))
        "object" -> (VObject . map fieldToTuple) <$> parens (braces (sepEndBy fieldParser (lexeme $ char ',')))
        -- NEW Cases
        "optional" -> VOptional <$> parens convexTypeParser
        "union" -> VUnion <$> parens (sepEndBy convexTypeParser (lexeme $ char ','))
        "literal" -> VLiteral <$> parens stringLiteral
        _ -> fail $ "Unknown v-dot type: " ++ typeName

    -- Parses a reference to a constant, like `roleEnum` or `productEnum`.
    referenceParser = VReference <$> identifier

topLevelConstParser :: SchemaParser ()
topLevelConstParser = lexeme $ do
  void $ try (reserved "export") -- `export` is optional
  reserved "const"
  constName <- identifier
  void $ lexeme $ char '='
  constType <- convexTypeParser
  void $ lexeme $ char ';' <|> (lookAhead (char '\n')) -- End with semicolon or newline
  -- Modify the state by inserting the new constant
  modifyState (\s -> s {psConstants = Map.insert constName constType (psConstants s)})

ignoredStatementParser :: SchemaParser ()
ignoredStatementParser =
  choice . map try $
    [ -- Skips over import statements
      importStatement,
      -- Skips over `type Role = ...;`
      typeAliasStatement,
      -- Skips over `const _: Role = ...;`
      typeAssertionStatement,
      -- Skips over `const ROLE_HIERARCHY = {...} as const;`
      constObjectStatement
    ]
  where
    importStatement = do
      reserved "import"
        *> manyTill anyChar (char ';')
        *> pure ()
    typeAliasStatement = do
      (void (try (reserved "export")) *> reserved "type")
        *> manyTill anyChar (char ';')
        *> pure ()
    typeAssertionStatement = do
      reserved "const"
        *> char '_'
        *> manyTill anyChar (char ';')
        *> pure ()
    constObjectStatement = do
      (void (try (reserved "export")) *> reserved "const")
        *> identifier
        *> lexeme (char '=')
        *> (braces (many (noneOf "}")) *> whiteSpace)
        *> manyTill anyChar (char ';')
        *> pure ()

fileParser :: SchemaParser ParsedFile
fileParser = do
  whiteSpace
  -- In a loop, try to parse an ignored statement or a constant definition.
  -- This continues until the main schema definition is found.
  _ <- many (try (ignoredStatementParser <* whiteSpace) <|> try (topLevelConstParser <* whiteSpace))

  -- Parse the main schema definition block.
  reserved "export"
  reserved "default"
  reserved "defineSchema"
  tables <- parens $ braces $ sepEndBy tableParser (lexeme $ char ',')

  -- After everything is parsed, get the final state.
  finalState <- getState
  return $ ParsedFile (psConstants finalState) (Schema tables)

parseSchema :: String -> IO (Either ParseError ParsedFile)
parseSchema input = do
  -- `runParserT` takes the parser, state, filename, and input.
  -- It returns the result and the final state.
  result <- runParserT fileParser initialState "(schema.ts)" input
  case result of
    Left err -> return $ Left err
    Right res -> return $ Right res
