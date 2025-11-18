{-# LANGUAGE DuplicateRecordFields #-}
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
    ParserState (..),
    initialState,
    getLiteralString,
    isLiteral,
    sanitizeUnionValues,
  )
where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Text.Parsec
import qualified Text.Parsec.Language as Token
import qualified Text.Parsec.Token as Token

type SchemaParser a = ParsecT String ParserState IO a

data ParserState = ParserState
  { psConstants :: Map String ConvexType
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

data Index
  = Index
      { indexName :: String,
        indexFields :: [String]
      }
  | VectorIndex
      { indexName :: String,
        indexVectorField :: String,
        indexDimensions :: Int,
        indexFilterFields :: [String]
      }
  | SearchIndex
      { indexName :: String,
        indexSearchField :: String,
        indexFilterFields :: [String],
        indexStaged :: Bool
      }
  deriving (Show, Eq)

data SearchIndexOption
  = SearchFieldOption String
  | SearchFilterFieldsOption [String]
  | SearchStagedOption Bool

data VectorIndexOption
  = VectorFieldOption String
  | DimensionsOption Int
  | FilterFieldsOption [String]

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
  = VString
  | VNumber
  | VInt64
  | VFloat64
  | VBoolean
  | VBytes
  | VNull
  | VAny
  | VId String
  | VArray ConvexType
  | VObject [(String, ConvexType)]
  | VOptional ConvexType
  | VUnion [ConvexType]
  | VLiteral String
  | VReference String
  | VVoid
  deriving (Show, Eq, Ord)

getLiteralString :: ConvexType -> String
getLiteralString (VLiteral str) = str
getLiteralString _ = error "Expected a literal type"

isLiteral :: ConvexType -> Bool
isLiteral (VLiteral _) = True
isLiteral _ = False

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
          "defineSchema(",
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

topLevelStatementEnd :: SchemaParser ()
topLevelStatementEnd = void (optional (lexeme (char ';'))) *> whiteSpace

itemEnd :: SchemaParser ()
itemEnd = do
  optional (lexeme (char ','))
  optional (lexeme (char ';'))
  whiteSpace

fieldToTuple :: Field -> (String, ConvexType)
fieldToTuple (Field name typ) = (name, typ)

fieldParser :: SchemaParser Field
fieldParser = lexeme $ do
  key <- identifier <|> stringLiteral
  void $ lexeme $ char ':'
  value <- convexTypeParser
  return $ Field key value

-- After parsing a ConvexType, optionally swallow a trailing TS-style cast:
--   v.string() as IsbnValidator
-- We ignore the type on the Haskell side.
withOptionalAs :: SchemaParser ConvexType -> SchemaParser ConvexType
withOptionalAs p = do
  t <- p
  _ <- optional $ try $ do
    whiteSpace
    -- TS 'as' keyword (we didn't add it as reserved, so use a raw string)
    _ <- lexeme (string "as")
    -- Extremely simple type parser: just an identifier or string literal.
    -- This is enough for `as IsbnValidator` or `as "foo"`.
    _ <- lexeme (identifier <|> stringLiteral)
    return ()
  return t

indexParser :: SchemaParser Index
indexParser = lexeme $ do
  void $ char '.'
  indexType <- identifier
  case indexType of
    "index" -> parseScalarIndex
    "vectorIndex" -> parseVectorIndex
    "searchIndex" -> parseSearchIndex
    _ -> fail $ "Unknown index type: " ++ indexType
  where
    parseScalarIndex = do
      (iName, iFields) <- parens $ do
        name <- stringLiteral
        void $ lexeme $ char ','
        fields <- brackets $ sepEndBy stringLiteral (lexeme $ char ',')
        return (name, fields)
      return $ Index iName iFields

    parseVectorIndex = do
      (iName, vectorField, dimensions, filterFields) <- parens $ do
        name <- stringLiteral
        void $ lexeme $ char ','
        (vecField, dims, filters) <- vectorIndexOptionsParser
        return (name, vecField, dims, filters)
      return $
        VectorIndex
          { indexName = iName,
            indexVectorField = vectorField,
            indexDimensions = dimensions,
            indexFilterFields = filterFields
          }

    parseSearchIndex = do
      (iName, searchField, filterFields, staged) <- parens $ do
        name <- stringLiteral
        void $ lexeme $ char ','
        (sField, fFields, isStaged) <- searchIndexOptionsParser
        return (name, sField, fFields, isStaged)
      return $
        SearchIndex
          { indexName = iName,
            indexSearchField = searchField,
            indexFilterFields = filterFields,
            indexStaged = staged
          }

    searchIndexOptionsParser :: SchemaParser (String, [String], Bool)
    searchIndexOptionsParser = do
      opts <- braces $ sepEndBy searchOption (lexeme $ char ',')
      let searchFields = [field | SearchFieldOption field <- opts]
          filterFields = [fs | SearchFilterFieldsOption fs <- opts]
          stagedValues = [staged | SearchStagedOption staged <- opts]
      -- Only searchField is required.
      searchField <-
        case searchFields of
          [field] -> return field
          [] -> fail "searchIndex is missing required option 'searchField'"
          _ -> fail "searchIndex received multiple 'searchField' options"
      filterFieldsFinal <-
        case filterFields of
          [] -> return []
          [fs] -> return fs
          _ -> fail "searchIndex received multiple 'filterFields' options"
      stagedFinal <-
        case stagedValues of
          [staged] -> return staged
          [] -> return False
          _ -> fail "searchIndex received multiple 'staged' options"
      return (searchField, filterFieldsFinal, stagedFinal)

    searchOption :: SchemaParser SearchIndexOption
    searchOption = lexeme $ do
      key <- identifier <|> stringLiteral
      void $ lexeme $ char ':'
      case key of
        "searchField" -> SearchFieldOption <$> stringLiteral
        "filterFields" -> SearchFilterFieldsOption <$> brackets (sepEndBy stringLiteral (lexeme $ char ','))
        "staged" -> SearchStagedOption <$> (lexeme (string "true" >> return True) <|> (string "false" >> return False))
        _ -> fail $ "Unknown searchIndex option: " ++ key

    vectorIndexOptionsParser :: SchemaParser (String, Int, [String])
    vectorIndexOptionsParser = do
      opts <- braces $ sepEndBy vectorOption (lexeme $ char ',')
      let vectorFields = [field | VectorFieldOption field <- opts]
          dimensionValues = [dim | DimensionsOption dim <- opts]
          filters = [fs | FilterFieldsOption fs <- opts]
      vectorField <-
        case vectorFields of
          [field] -> return field
          [] -> fail "vectorIndex is missing required option 'vectorField'"
          _ -> fail "vectorIndex received multiple 'vectorField' options"
      dimensions <-
        case dimensionValues of
          [dim] -> return dim
          [] -> fail "vectorIndex is missing required option 'dimensions'"
          _ -> fail "vectorIndex received multiple 'dimensions' options"
      filterFields <-
        case filters of
          [] -> return []
          [fs] -> return fs
          _ -> fail "vectorIndex received multiple 'filterFields' options"
      return (vectorField, dimensions, filterFields)

    vectorOption :: SchemaParser VectorIndexOption
    vectorOption = lexeme $ do
      key <- identifier <|> stringLiteral
      void $ lexeme $ char ':'
      case key of
        "vectorField" -> VectorFieldOption <$> stringLiteral
        "dimensions" -> DimensionsOption . fromInteger <$> Token.integer lexer
        "filterFields" -> FilterFieldsOption <$> brackets (sepEndBy stringLiteral (lexeme $ char ','))
        _ -> fail $ "Unknown vectorIndex option: " ++ key

tableParser :: SchemaParser Table
tableParser = lexeme $ do
  tName <- identifier <|> stringLiteral
  void $ lexeme $ char ':'
  reserved "defineTable"
  fields <- parens $ do
    tableDef <-
      (try (VObject . map fieldToTuple <$> braces (sepEndBy fieldParser (lexeme $ char ','))))
        <|> (VReference <$> identifier)
    case tableDef of
      VReference refName -> trace ("refName parsed: " ++ show refName) $ do
        st <- getState
        trace ("psConstants keys: " ++ show (Map.keys (psConstants st))) $
          case Map.lookup refName (psConstants st) of
            Just (VObject fs) -> return $ map (\(n, t) -> Field n t) fs
            _ -> fail $ "Table '" ++ tName ++ "' references an unknown or non-object constant: " ++ refName
      VObject fs -> return $ map (\(n, t) -> Field n t) fs
      _ -> fail "Invalid table definition: expected an object or a reference."

  -- After parsing defineTable(...), now look for zero or more chained .index() calls.
  indexes <- many indexParser

  itemEnd
  return $ Table tName fields indexes

structParser :: SchemaParser ConvexType
structParser = do
  res <- VObject . map fieldToTuple <$> braces (sepEndBy fieldParser (lexeme $ char ','))
  itemEnd
  return res

convexTypeParser :: SchemaParser ConvexType
convexTypeParser = withOptionalAs convexTypeCore

convexTypeCore :: SchemaParser ConvexType
convexTypeCore =
  choice . map try $
    [ vParser,
      structParser,
      referenceParser
    ]
  where
    vParser = do
      void $ lexeme $ reserved "v"
      void $ lexeme $ char '.'
      typeName <- identifier
      case typeName of
        "string" -> VString <$ parens (return ())
        "number" -> VNumber <$ parens (return ())
        "boolean" -> VBoolean <$ parens (return ())
        "bytes" -> VBytes <$ parens (return ())
        "int64" -> VInt64 <$ parens (return ())
        "float64" -> VFloat64 <$ parens (return ())
        "null" -> VNull <$ parens (return ())
        "any" -> VAny <$ parens (return ())
        "id" -> VId <$> parens stringLiteral
        "array" -> VArray <$> parens convexTypeParser
        "object" -> parens structParser
        "optional" -> VOptional <$> parens convexTypeParser
        "union" -> VUnion <$> parens (sepEndBy convexTypeParser (lexeme $ char ','))
        "literal" -> VLiteral <$> parens stringLiteral
        _ -> fail $ "Unknown v-dot type: " ++ typeName

    referenceParser = VReference <$> identifier

-- | Sanitizes union literals. It might be that a union like this is defined:
-- export const instruction_mime_type = v.union(
--   v.literal("application/pdf"),
--   v.literal("text/html"),
--   v.literal("text/plain")
-- );
--
-- And `application/pdf` would be translated into a type `Application/pdf`, which
-- is invalid in most languages. After sanitization, it would become `application_pdf`.
sanitizeUnionValues :: String -> String
sanitizeUnionValues = concatMap (\c -> if c `elem` ['/', '@', '\\'] then ['_'] else [c])

topLevelConstParser :: SchemaParser ()
topLevelConstParser = lexeme $ do
  void $ optional (reserved "export")
  reserved "const"
  constName <- identifier
  void $ lexeme $ char '='
  constType <-
    try (reserved "defineTable" *> (VObject . map fieldToTuple <$> parens (braces (many fieldParser))))
      <|> convexTypeParser
  topLevelStatementEnd
  modifyState (\s -> s {psConstants = Map.insert constName constType (psConstants s)})

topLevelTypeParser :: SchemaParser ()
topLevelTypeParser = lexeme $ do
  void $ optional (reserved "export")
  reserved "type"
  typeName <- identifier
  void $ lexeme $ char '='
  optional (reserved "typeof")
  refType <- convexTypeParser
  topLevelStatementEnd
  modifyState (\s -> s {psConstants = Map.insert typeName refType (psConstants s)})

parseSchema :: String -> IO (Either ParseError ParsedFile)
parseSchema input = do
  -- First Pass: Collect all top-level definitions (consts and types).
  let definitionsPassParser = many (try topLevelConstParser <|> try topLevelTypeParser <|> (anyChar >> return ()))
  constsState <- execParser (definitionsPassParser *> getState) initialState "(schema.ts)" input

  -- Second Pass: Parse the schema, using the constants we just found.
  let schemaPassParser = do
        _ <- manyTill anyChar (lookAhead (try (reserved "defineSchema(")))
        reserved "defineSchema"
        tables <- parens $ braces $ many tableParser
        return $ Schema tables

  schemaResult <- execParser schemaPassParser constsState "(schema.ts)" input

  return $ Right (ParsedFile (psConstants constsState) schemaResult)

-- | A helper to run a parser and return the result, simplifying error handling.
execParser :: SchemaParser a -> ParserState -> SourceName -> String -> IO a
execParser p st name input = do
  result <- runParserT p st name input
  case result of
    Left err -> fail (show err)
    Right res -> return res
