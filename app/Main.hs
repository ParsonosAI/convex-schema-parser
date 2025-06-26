{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Backend.Python (generatePythonCode)
import Convex.Schema.Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let filePath = if null args then "example/schema.ts" else head args
  fileContent <- readFile filePath

  putStrLn "--- Parsing Convex Schema ---"
  putStrLn "Input:"
  putStrLn fileContent
  putStrLn "-----------------------------"

  parseSchema fileContent >>= \case
    Left err -> do
      putStrLn "Parsing failed:"
      print err
    Right schema -> do
      putStrLn "Parsing successful! ✅"
      putStrLn "Generated AST..., generating python module"
      writeFile "schema.py" (generatePythonCode schema)
      putStrLn "Successfully generated python module"
