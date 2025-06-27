-- app/Main.hs
module Main (main) where

import qualified Backend.Python as Python
import qualified Convex.Parser as P
import Data.List (intercalate)
import Options.Applicative
import System.Exit (exitFailure)

data GenTarget = Python | Rust deriving (Show, Eq, Enum, Bounded)

availableTargets :: String
availableTargets = "Available: " ++ intercalate ", " (map show [minBound :: GenTarget .. maxBound :: GenTarget])

instance Read GenTarget where
  readsPrec _ "Python" = [(Python, "")]
  readsPrec _ "Rust" = [(Rust, "")]
  readsPrec _ _ = []

data CliOptions = CliOptions
  { schemaPath :: FilePath,
    declarationsDir :: FilePath,
    target :: GenTarget
  }

cliOptionsParser :: Parser CliOptions
cliOptionsParser =
  CliOptions
    <$> strOption
      ( long "schema"
          <> metavar "SCHEMA_PATH"
          <> help "Path to the source schema.ts file"
      )
    <*> strOption
      ( long "declarations"
          <> metavar "DECL_DIR"
          <> help "Path to the root of the tsc-generated declarations directory"
      )
    <*> option
      auto
      ( long "target"
          <> metavar "TARGET"
          <> value Python
          <> help ("Target language for code generation " ++ availableTargets)
      )

main :: IO ()
main = do
  opts <- customExecParser p optsParserInfo
  projectResult <- P.parseProject (schemaPath opts) (declarationsDir opts)
  case projectResult of
    Left err -> do
      putStrLn $ "Error parsing project: " ++ err
      exitFailure
    Right project ->
      case target opts of
        Python -> do
          let pythonCode = Python.generatePythonCode project
          putStrLn pythonCode
        Rust ->
          putStrLn "Rust backend not yet implemented."
  where
    optsParserInfo =
      info
        (cliOptionsParser <**> helper)
        ( fullDesc
            <> progDesc "Generate typed clients from a Convex schema and function definitions."
            <> header "convex-parser - A code generator for Convex"
        )
    p = prefs showHelpOnError
