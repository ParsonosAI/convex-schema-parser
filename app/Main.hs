-- app/Main.hs
module Main (main) where

import qualified Backend.Python as Python
import qualified Convex.Parser as P
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [schemaPath, declRootDir] -> do
      -- Run the main orchestrator parser
      projectResult <- P.parseProject schemaPath declRootDir
      case projectResult of
        Left err -> do
          putStrLn $ "Error parsing project: " ++ err
          exitFailure
        Right project -> do
          -- Pass the complete project AST to the Python backend
          let pythonCode = Python.generatePythonCode project
          -- Write the final result to `convex.py`
          writeFile "convex.py" pythonCode
    _ -> do
      putStrLn "Usage: convex-schema-parser <path-to-schema.ts> <path-to-declarations-root>"
      exitFailure
