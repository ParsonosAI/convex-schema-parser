{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Convex.Action.Parser
  ( ConvexFunction (..),
    FuncType (..),
    parseActionFile,
    Schema.ConvexType (VVoid),
  )
where

import Control.Monad (void)
import Convex.Schema.Parser (parens)
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
          "void",
          "GenericId",
          "DefaultFunctionArgs"
        ],
      Token.caseSensitive = True
    }

lexer :: Token.GenTokenParser String Schema.ParserState IO
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

braces :: SchemaParser a -> SchemaParser a
braces = Token.braces lexer

angles :: SchemaParser a -> SchemaParser a
angles p = lexeme (char '<') *> p <* lexeme (char '>')

dtsTypeParser :: SchemaParser Schema.ConvexType
dtsTypeParser = do
  -- A type can be a union of other types
  types <- sepBy1 singleType (lexeme (char '|'))
  let baseType = if length types == 1 then head types else Schema.VUnion types
  -- After parsing the base type, check for array suffixes `[]`
  arrayCount <- length <$> many (lexeme (string "[]"))
  -- Wrap the base type in VArray for each `[]` found
  return $ foldr (\_ acc -> Schema.VArray acc) baseType (replicate arrayCount ())
  where
    singleType =
      (Schema.VString <$ try (reserved "string"))
        <|> (Schema.VNumber <$ try (reserved "number"))
        <|> (Schema.VBoolean <$ try (reserved "boolean"))
        <|> (Schema.VAny <$ try (reserved "any"))
        <|> (Schema.VLiteral <$> try stringLiteral)
        <|> (Schema.VId <$> try genericIdParser)
        <|> (Schema.VObject <$> try (braces (sepEndBy dtsFieldParser (lexeme (char ';')))))
        <|> (Schema.VReference <$> identifier)

-- A parser for a single field inside an argument or object type
dtsFieldParser :: SchemaParser (String, Schema.ConvexType)
dtsFieldParser = lexeme $ do
  name <- identifier
  isOptional <- optionMaybe (lexeme (char '?'))
  void $ lexeme $ char ':'
  typ <- dtsTypeParser
  -- If the `?` was present, wrap the final type in VOptional
  let finalType = maybe typ (const $ Schema.VOptional typ) isOptional
  return (name, finalType)

-- A parser for `import("...").GenericId<"...">`
genericIdParser :: SchemaParser String
genericIdParser = do
  void $ reserved "import"
  void $ parens stringLiteral
  void $ lexeme $ char '.'
  void $ reserved "GenericId"
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
  optional (try (lexeme (string "/**") *> manyTill anyChar (try (string "*/"))))
  whiteSpace

  reserved "export"
  reserved "declare"
  reserved "const"
  fName <- identifier
  void $ lexeme $ char ':'

  void $ reserved "import"
  void $ parens stringLiteral
  void $ lexeme $ char '.'

  fTypeStr <-
    choice
      [ try (reserved "RegisteredQuery" >> return "RegisteredQuery"),
        try (reserved "RegisteredMutation" >> return "RegisteredMutation"),
        try (reserved "RegisteredAction" >> return "RegisteredAction")
      ]

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
      (braces (sepEndBy dtsFieldParser (lexeme (char ';'))))
        <|> (try defaultFuncArgsParser $> [])
    void $ lexeme $ char ','
    void $ reserved "Promise"
    ret <- angles ((reserved "void" $> Schema.VVoid) <|> dtsTypeParser)
    return (vis, args, ret)

  void $ lexeme $ char ';'

  if visibility == "internal"
    then return Nothing
    else return $ Just (ConvexFunction fName fPath fType fArgs fReturn)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x : xs) =
  case f x of
    Just v -> v : mapMaybe f xs
    Nothing -> mapMaybe f xs

parseActionFile :: String -> SchemaParser [ConvexFunction]
parseActionFile path = whiteSpace *> (mapMaybe id <$> many (try (registeredFunctionParser path)))
