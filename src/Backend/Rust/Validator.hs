{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Backend.Rust.Validator (run, RustValidator (..), RustValidatorEnv (..)) where

import Control.Monad.Reader
import Convex.Validator
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hGetContents)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)

newtype RustValidator a = RustValidator
  { runRustValidator :: ReaderT RustValidatorEnv IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader RustValidatorEnv, MonadFail)

data RustValidatorEnv = RustValidatorEnv {projectPath :: FilePath}

run :: RustValidatorEnv -> RustValidator a -> IO a
run renv action = runReaderT (runRustValidator action) renv

instance Validator RustValidator where
  -- \| Sets up the Rust validation sandbox project.
  setup = do
    config <- ask
    let rustProjectPath = projectPath config
    -- Create the directory structure.
    liftIO $ createDirectoryIfMissing True (rustProjectPath </> "src")

    -- Write the Cargo.toml file if it doesn't exist.
    let cargoTomlPath = rustProjectPath </> "Cargo.toml"
    cargoTomlExists <- liftIO $ doesFileExist cargoTomlPath
    if not cargoTomlExists
      then liftIO $ writeFile cargoTomlPath cargoTomlContent
      else return ()

    -- Write the src/lib.rs file if it doesn't exist.
    let libRsPath = rustProjectPath </> "src" </> "lib.rs"
    libRsExists <- liftIO $ doesFileExist libRsPath
    if not libRsExists
      then liftIO $ writeFile libRsPath libRsContent
      else return ()

  -- \| Validates the generated Rust code.
  validate generatedCode = do
    rustProjectPath <- asks projectPath
    let generatedFilePath = rustProjectPath </> "src" </> "generated_api.rs"

    -- Write the generated code to the sandbox project.
    liftIO $ writeFile generatedFilePath generatedCode

    -- Run `cargo fmt` to format the code.
    liftIO $ putStrLn "[Validator] Running 'cargo fmt'..."
    let fmtCmd = (proc "cargo" ["fmt"]) {cwd = Just rustProjectPath}
    (_, _, _, fmtHandle) <- liftIO $ createProcess fmtCmd
    fmtExitCode <- liftIO $ waitForProcess fmtHandle

    if fmtExitCode /= ExitSuccess
      then do
        liftIO $ putStrLn "[Validator] Error: 'cargo fmt' failed. The generated code has syntax errors."
        return Nothing
      else do
        -- Run `cargo check` to validate types and ownership.
        liftIO $ putStrLn "[Validator] Running 'cargo check'..."
        let checkCmd = (proc "cargo" ["check"]) {cwd = Just rustProjectPath, std_err = CreatePipe}
        (_, _, Just stdErr, checkHandle) <- liftIO $ createProcess checkCmd
        checkExitCode <- liftIO $ waitForProcess checkHandle

        if checkExitCode == ExitSuccess
          then do
            liftIO $ putStrLn "[Validator] Validation successful."
            -- Read file content from the checked and formatted file.
            content <- liftIO $ readFile generatedFilePath
            return $ Just content
          else do
            liftIO $ putStrLn "[Validator] Error: 'cargo check' failed. The generated code is invalid."
            liftIO $ hGetContents stdErr >>= putStrLn
            return Nothing

-- | The content for the validation project's Cargo.toml file.
cargoTomlContent :: String
cargoTomlContent =
  unlines
    [ "[package]",
      "name = \"validation-project\"",
      "version = \"0.1.0\"",
      "edition = \"2021\"",
      "",
      "[dependencies]",
      "convex = \"0.9.0\"",
      "serde = { version = \"1\", features = [\"derive\"] }",
      "serde_json = \"1\"",
      "thiserror = \"1.0\"",
      "anyhow = \"1.0\"",
      "futures-util = { version = \"0.3\" }",
      "tokio = { version = \"1\", features = [\"full\"] }"
    ]

-- | The content for the validation project's src/lib.rs file.
libRsContent :: String
libRsContent = "pub mod generated_api;\n"
