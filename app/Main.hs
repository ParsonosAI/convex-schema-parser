module Main (main) where

import qualified Backend.Python as Python
import qualified Backend.Rust as Rust
import qualified Config
import qualified Convex.Parser as P
import Data.List (intercalate)
import Dev (runDevMode)
import qualified Dev
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit (exitFailure)

data Command
  = Generate GenerateOptions
  | Dev DevCliOptions

data GenerateOptions = GenerateOptions
  { schemaPath :: FilePath,
    declarationsDir :: FilePath,
    target :: Config.GenTarget,
    outputFile :: Maybe FilePath
  }

data DevCliOptions = DevCliOptions
  { projectPath :: FilePath,
    configPath :: FilePath
  }

-- We can reuse the GenTarget Read instance from Config.hs if it were exported,
-- but redefining it here keeps Main self-contained for parsing.
instance Read Config.GenTarget where
  readsPrec _ "Python" = [(Config.Python, "")]
  readsPrec _ "Rust" = [(Config.Rust, "")]
  readsPrec _ _ = []

availableTargets :: String
availableTargets = "Available: " ++ intercalate ", " (map show [(minBound :: Config.GenTarget) ..])

generateOptionsParser :: Parser GenerateOptions
generateOptionsParser =
  GenerateOptions
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
          <> value Config.Python
          <> showDefault
          <> help ("Target language for code generation. " ++ availableTargets)
      )
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "OUTPUT_FILE"
              <> help "File to write the generated code to (prints to stdout if omitted)"
          )
      )

devCliOptionsParser :: Parser DevCliOptions
devCliOptionsParser =
  DevCliOptions
    <$> strOption
      ( long "path"
          <> metavar "PROJECT_PATH"
          <> help "Path to the root of your convex project directory"
      )
    <*> strOption
      ( long "config"
          <> metavar "CONFIG_FILE"
          <> value "convex-parser.yaml"
          <> showDefault
          <> help "Path to the YAML config file"
      )

commandParser :: Parser Command
commandParser =
  subparser
    ( command "generate" (info (Generate <$> generateOptionsParser) (progDesc "Generate a client once and exit"))
        <> command "dev" (info (Dev <$> devCliOptionsParser) (progDesc "Watch for changes and regenerate clients from a config file"))
    )

main :: IO ()
main = do
  cmd <- customExecParser p optsParserInfo
  case cmd of
    Generate opts -> runGenerate opts
    Dev opts -> runDev opts
  where
    optsParserInfo =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Generate typed clients from a Convex schema and function definitions."
            <> header "convex-parser - A code generator for Convex"
        )
    p = prefs showHelpOnError

runGenerate :: GenerateOptions -> IO ()
runGenerate opts = do
  projectResult <- P.parseProject (schemaPath opts) (declarationsDir opts)
  case projectResult of
    Left err -> do
      putStrLn $ "Error parsing project: " ++ err
      exitFailure
    Right project ->
      let generatedCode = case target opts of
            Config.Python -> Python.generatePythonCode project
            Config.Rust -> Rust.generateRustCode project
       in case outputFile opts of
            Just path -> writeFile path generatedCode
            Nothing -> putStrLn generatedCode

runDev :: DevCliOptions -> IO ()
runDev opts = do
  configExists <- doesFileExist (configPath opts)
  if not configExists
    then do
      putStrLn $ "Error: Config file not found at " ++ configPath opts
      exitFailure
    else do
      configResult <- Config.loadConfig (configPath opts)
      case configResult of
        Left err -> do
          putStrLn "Error parsing config file:"
          print err
          exitFailure
        Right config ->
          let declarationsDir = projectPath opts ++ "/tmp/declarations"
              devOpts = Dev.DevOptions (projectPath opts) declarationsDir config
           in runDevMode devOpts
