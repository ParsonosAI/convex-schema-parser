{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Convex.Action.Parser
  ( ConvexFunction (..),
    DTSType (..),
    FuncType (..),
    parseActionFile,
    registeredTypesParser,
    Schema.ConvexType (VVoid),
  )
where

import Control.Monad (void)
import qualified Convex.Schema.Parser as Schema
import Data.Functor (($>))
import Text.Parsec
import qualified Text.Parsec.Token as Token

type SchemaParser a = ParsecT String Schema.ParserState IO a

data FuncType = Query | Mutation | Action
  deriving (Show, Eq)

data ConvexFunction = ConvexFunction
  { funcName :: String,
    funcPath :: String,
    funcType :: FuncType,
    funcArgs :: [(String, Schema.ConvexType)],
    funcReturn :: Schema.ConvexType
  }
  deriving (Show, Eq)

data DTSType = DTSType
  { dtsTypeName :: String,
    dtsTypeFields :: [(String, Schema.ConvexType)]
  }
  deriving (Show, Eq)

-- Slightly different lexer for Actions.
langDef :: Token.GenLanguageDef String Schema.ParserState IO
langDef =
  Token.LanguageDef
    { Token.commentStart = "/*",
      Token.commentEnd = "*/",
      Token.commentLine = "//",
      Token.nestedComments = True,
      Token.identStart = letter <|> char '_',
      Token.identLetter = alphaNum <|> char '_',
      Token.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
      Token.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      Token.reservedOpNames = [],
      Token.reservedNames =
        [ "export",
          "declare",
          "const",
          "import",
          "from",
          "RegisteredQuery",
          "RegisteredMutation",
          "RegisteredAction",
          "Promise",
          "any",
          "string",
          "number",
          "boolean",
          "true",
          "false",
          "never",
          "null",
          "undefined",
          "void",
          "GenericId",
          "Id",
          "DefaultFunctionArgs",
          "ArrayBuffer",
          "bigint"
        ],
      Token.caseSensitive = True
    }

lexer :: Token.GenTokenParser String Schema.ParserState IO
lexer = Token.makeTokenParser langDef

parens :: SchemaParser a -> SchemaParser a
parens = Token.parens lexer

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

braces :: SchemaParser a -> SchemaParser a
braces = Token.braces lexer

angles :: SchemaParser a -> SchemaParser a
angles p = lexeme (char '<') *> p <* lexeme (char '>')

dtsTypeParser :: SchemaParser Schema.ConvexType
dtsTypeParser = do
  -- TypeScript postfix arrays bind more tightly than intersections and unions:
  -- `string[] | undefined` means `(string[]) | undefined`, while
  -- `(string | number)[]` remains expressible through the parenthesized parser.
  unions <- sepBy1 intersectionTypeParser (lexeme (char '|'))
  return $ case unions of
    [single] -> single
    _ -> Schema.VUnion unions
  where
    -- Parses both single identifiers (like `RoleEnum`)
    -- and qualified identifiers (like `Stripe.Subscription`).
    qualifiedIdentifierParser :: SchemaParser Schema.ConvexType
    qualifiedIdentifierParser = do
      parts <- sepBy1 identifier (lexeme (char '.'))
      if length parts > 1
        then -- If there's a dot, it's definitely an external type.
          return Schema.VAny
        else -- Otherwise, it's a single-word identifier, treat as a reference.
          return (Schema.VReference (head parts))

    -- intersection of postfix types, e.g. `string & { _: "isbn" }`
    intersectionTypeParser :: SchemaParser Schema.ConvexType
    intersectionTypeParser = do
      parts <- sepBy1 postfixTypeParser (lexeme (char '&'))
      -- We deliberately ignore all but the first component, to drop branding like:
      --   string & { _: "isbn" }
      -- and just keep the base type (`string`).
      -- If you ever need smarter logic, you can refine this combine function.
      return (head parts)

    postfixTypeParser = do
      baseType <- singleType
      arrayCount <- length <$> many (lexeme (string "[]"))
      return $ foldr (\_ acc -> Schema.VArray acc) baseType (replicate arrayCount ())

    singleType =
      (Schema.VString <$ try (reserved "string"))
        <|> (Schema.VNumber <$ try (reserved "number"))
        <|> (Schema.VBoolean <$ try (reserved "boolean"))
        <|> (Schema.VBoolean <$ try (reserved "true"))
        <|> (Schema.VBoolean <$ try (reserved "false"))
        <|> (Schema.VAny <$ try (reserved "never"))
        <|> (Schema.VNull <$ try (reserved "null"))
        <|> (Schema.VVoid <$ try (reserved "undefined"))
        <|> (Schema.VBytes <$ try (reserved "ArrayBuffer"))
        <|> (Schema.VInt64 <$ try (reserved "bigint"))
        <|> (Schema.VAny <$ try (reserved "any"))
        <|> (Schema.VLiteral <$> try stringLiteral)
        <|> (Schema.VId <$> try genericIdParser)
        <|> (Schema.VId <$> try bareIdParser)
        <|> (Schema.VObject <$> try (braces (sepEndBy dtsFieldParser (lexeme (char ';')))))
        <|> try (parens dtsTypeParser)
        <|> qualifiedIdentifierParser

-- A parser for a single field inside an argument or object type
dtsFieldParser :: SchemaParser (String, Schema.ConvexType)
dtsFieldParser = lexeme $ do
  -- Ignore potential comments before the field starting // or /** */
  optional (try (lexeme (string "/**") *> manyTill anyChar (try (string "*/"))))
  void $ many (try (lexeme (string "//" *> manyTill anyChar (try newline))))
  whiteSpace

  name <- identifier
  isOptional <- optionMaybe (lexeme (char '?'))
  void $ lexeme $ char ':'
  typ <- dtsTypeParser
  let (containsUndefined, withoutUndefined) = removeUndefined typ
      finalType =
        if containsUndefined || maybe False (const True) isOptional
          then Schema.VOptional withoutUndefined
          else withoutUndefined
  return (name, finalType)
  where
    removeUndefined (Schema.VUnion types) =
      let retained = filter (/= Schema.VVoid) types
          normalized = case retained of
            [singleType] -> singleType
            _ -> Schema.VUnion retained
       in (length retained /= length types, normalized)
    removeUndefined Schema.VVoid = (True, Schema.VAny)
    removeUndefined otherType = (False, otherType)

-- A parser for `import("...").GenericId<"...">`
genericIdParser :: SchemaParser String
genericIdParser = do
  void $ reserved "import"
  void $ parens stringLiteral
  void $ lexeme $ char '.'
  void $ reserved "GenericId"
  angles stringLiteral

bareIdParser :: SchemaParser String
bareIdParser = do
  void $ lexeme $ string "Id"
  angles stringLiteral

-- A parser for `import("...").DefaultFunctionArgs`
defaultFuncArgsParser :: SchemaParser ()
defaultFuncArgsParser = do
  void $ reserved "import"
  void $ parens stringLiteral
  void $ lexeme $ char '.'
  void $ reserved "DefaultFunctionArgs"

registeredFunctionParser :: String -> SchemaParser (Maybe ConvexFunction)
registeredFunctionParser fPath = lexeme $ do
  -- Backtrack only while recognizing the declaration header. Once a known
  -- Registered* export is recognized, malformed or unsupported types must be
  -- reported rather than silently consumed by skippedExportParser.
  (fName, fTypeStr) <- try $ do
    optional (try (lexeme (string "/**") *> manyTill anyChar (try (string "*/"))))
    whiteSpace

    reserved "export"
    reserved "declare"
    reserved "const"
    parsedName <- identifier
    void $ lexeme $ char ':'

    void $ reserved "import"
    void $ parens stringLiteral
    void $ lexeme $ char '.'

    parsedType <-
      choice
        [ try (reserved "RegisteredQuery" >> return "RegisteredQuery"),
          try (reserved "RegisteredMutation" >> return "RegisteredMutation"),
          try (reserved "RegisteredAction" >> return "RegisteredAction")
        ]
    return (parsedName, parsedType)

  let fType = case fTypeStr of
        "RegisteredQuery" -> Query
        "RegisteredMutation" -> Mutation
        "RegisteredAction" -> Action
        _ -> error "This case is unreachable due to the parser above"

  -- Parse the generic parameters
  (visibility, fArgs, fReturn) <- angles $ do
    vis <- stringLiteral
    void $ lexeme $ char ','
    args <-
      (try (braces (sepEndBy dtsFieldParser (lexeme (char ';')))))
        <|> (try defaultFuncArgsParser $> [])
    void $ lexeme $ char ','
    void $ reserved "Promise"
    ret <- angles ((reserved "void" $> Schema.VVoid) <|> dtsTypeParser)
    return (vis, args, ret)

  void $ lexeme $ char ';'

  case visibility of
    "public" -> return $ Just (ConvexFunction fName fPath fType fArgs fReturn)
    "internal" -> return Nothing
    other -> fail $ "Unknown or unhandled visibility in d.ts file: \"" ++ other ++ "\""

-- | Internal functions do not belong in generated clients. Recognize them by
-- their Registered* header, then skip the whole declaration without requiring
-- their argument or return types to be supported by the public-client grammar.
internalRegisteredFunctionParser :: SchemaParser ()
internalRegisteredFunctionParser = do
  visibility <- try . lookAhead $ do
    optional (try (lexeme (string "/**") *> manyTill anyChar (try (string "*/"))))
    whiteSpace
    reserved "export"
    reserved "declare"
    reserved "const"
    void identifier
    void $ lexeme $ char ':'
    void $ reserved "import"
    void $ parens stringLiteral
    void $ lexeme $ char '.'
    void $
      choice
        [ try (reserved "RegisteredQuery"),
          try (reserved "RegisteredMutation"),
          try (reserved "RegisteredAction")
        ]
    void $ lexeme $ char '<'
    stringLiteral
  if visibility == "internal"
    then skippedExportParser
    else parserZero

-- | Consumes exports that we don't know how to parse by respecting
-- nested braces/parens, so we don't stop at internal semicolons.
skippedExportParser :: SchemaParser ()
skippedExportParser = do
  reserved "export"
  reserved "declare"
  reserved "const"
  _ <- identifier
  void $ lexeme $ char ':'

  skipStmtRHS

  return ()

-- | Consumes the Right-Hand Side of a statement until the terminating semicolon.
-- It handles nested braces/parens recursively.
skipStmtRHS :: SchemaParser ()
skipStmtRHS = do
  -- Consume "top level" chunks until we hit the statement terminator
  void $ many topLevelElement
  void $ lexeme (char ';')
  where
    topLevelElement :: SchemaParser ()
    topLevelElement =
      choice
        [ void (try stringLiteral), -- Consume strings (they might contain ;)
          void (try (braces skipNested)), -- Recurse into objects { ... }
          void (try (parens skipNested)), -- Recurse into parens ( ... )
          void (noneOf ";") -- Consume anything else (space, identifiers, etc)
        ]

-- | Helper to skip content inside braces/parens.
-- It allows semicolons (because we are inside a block), but stops at the closing delimiter.
skipNested :: SchemaParser ()
skipNested = void $ many nestedElement
  where
    nestedElement :: SchemaParser ()
    nestedElement =
      choice
        [ void (try stringLiteral),
          void (try (braces skipNested)), -- Nested recursion {{ ... }}
          void (try (parens skipNested)),
          void (noneOf "})") -- Consume anything EXCEPT the closing brace/paren of the parent
        ]

-- | A helper to parse and ignore statements that we don't care about.
ignoredStatementParser :: SchemaParser ()
ignoredStatementParser =
  choice . map try $
    [ importStatement,
      lineComment,
      blockComment,
      void (skipMany1 (oneOf " \t\n\r"))
    ]
  where
    importStatement =
      reserved "import"
        *> manyTill anyChar (char ';')
        *> pure ()
    lineComment =
      string (Token.commentLine langDef) *> manyTill anyChar (try (lookAhead (char '\n'))) *> pure ()
    blockComment =
      string (Token.commentStart langDef)
        *> manyTill anyChar (try (string (Token.commentEnd langDef)))
        *> pure ()

registeredTypesParser :: SchemaParser (Maybe DTSType)
registeredTypesParser = lexeme $ do
  optional (try (lexeme (string "/**") *> manyTill anyChar (try (string "*/"))))
  whiteSpace

  -- Parse `[export] interface <Name> { ... }`
  void $ optional (try (reserved "export"))
  reserved "interface"
  typeName <- identifier
  fields <- braces (sepEndBy dtsFieldParser (lexeme (char ';')))
  return $ Just (DTSType typeName fields)

data ParsedDTs = ParsedFunction ConvexFunction | ParsedType DTSType deriving (Show, Eq)

parseActionFile :: String -> SchemaParser ([ConvexFunction], [DTSType])
parseActionFile path = do
  whiteSpace
  results <-
    many
      ( (try (internalRegisteredFunctionParser >> return Nothing))
          <|> (((ParsedFunction <$>) <$> registeredFunctionParser path))
          <|> (try ((ParsedType <$>) <$> registeredTypesParser))
          <|> (try (ignoredStatementParser >> return Nothing))
          <|> (try (skippedExportParser >> return Nothing))
      )
  -- Keep only Just values and separate functions from types
  let (funcs, types) = foldr separate ([], []) results
  return (funcs, types)
  where
    separate :: Maybe ParsedDTs -> ([ConvexFunction], [DTSType]) -> ([ConvexFunction], [DTSType])
    separate (Just (ParsedFunction func)) (fs, ts) = (func : fs, ts)
    separate (Just (ParsedType typ)) (fs, ts) = (fs, typ : ts)
    separate Nothing acc = acc
