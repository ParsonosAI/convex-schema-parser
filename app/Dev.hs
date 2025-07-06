{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dev (runDevMode, DevOptions (..)) where

import qualified Backend.Python as Python
import qualified Backend.Python.Validator as Python.Validator
import qualified Backend.Rust as Rust
import qualified Backend.Rust.Validator as Rust.Validator
import qualified Config
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import Control.DeepSeq (rnf)
import Control.Exception
import Control.Exception (evaluate)
import Control.Monad (forM_, forever, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Convex.Parser as P
import qualified Convex.Validator as Validator
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode (..), exitFailure)
import System.FSNotify
import System.IO.Error (isAlreadyInUseError)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)

data DevOptions = DevOptions
  { convexProjectPath :: FilePath,
    declarationsDir :: FilePath,
    config :: Config.Config
  }

-- | Searches for an available package manager, preferring pnpm.
findPackageManager :: IO (Maybe String)
findPackageManager = do
  pnpmPath <- findExecutable "pnpm"
  case pnpmPath of
    Just _ -> return (Just "pnpm")
    Nothing -> do
      npmPath <- findExecutable "npm"
      case npmPath of
        Just _ -> return (Just "npm")
        Nothing -> return Nothing

-- | Runs the development mode file watcher with debouncing.
runDevMode :: Config.Config -> IO ()
runDevMode config = do
  -- TVar to signal that a change has occurred.
  dirtyVar <- newTVarIO True -- Start dirty to trigger an initial run.

  -- Fork a worker thread that handles the generation.
  _ <- forkIO (workerThread dirtyVar config)

  -- The main thread will watch for file changes.
  withManager $ \mgr -> do
    putStrLn $ "Watching for changes in: " ++ convexPath
    watchTree
      mgr
      convexPath
      (const True) -- Watch all events
      ( \event -> do
          putStrLn $ "[Watcher] Detected change: " ++ showEvent event
          -- Atomically set the dirty flag. This is fast and non-blocking.
          atomically $ writeTVar dirtyVar True
      )

    -- Keep the main thread alive.
    forever $ threadDelay 1000000
  where
    convexPath = Config.projectPath config ++ "/convex"
    showEvent (Added path _ _) = "Added: " ++ path
    showEvent (Modified path _ _) = "Modified: " ++ path
    showEvent (Removed path _ _) = "Removed: " ++ path
    showEvent _ = "Unknown event"

-- | The worker thread loop that performs debouncing.
workerThread :: TVar Bool -> Config.Config -> IO ()
workerThread dirtyVar config = forever $ do
  -- Wait for a signal that something has changed.
  atomically $ do
    dirty <- readTVar dirtyVar
    -- The `check` function will retry the transaction if the condition is false.
    -- This effectively makes the thread sleep until `dirty` is True.
    when (not dirty) retry

  -- Once signaled, reset the flag and wait for the debounce period.
  atomically $ writeTVar dirtyVar False
  putStrLn "[Worker] Change detected. Debouncing for 300ms..."
  threadDelay 300000 -- 300ms debounce period.

  -- After debouncing, check if another change came in. If not, generate.
  isDirty <- atomically (readTVar dirtyVar)
  if isDirty
    then putStrLn "[Worker] Further changes detected. Restarting debounce timer."
    else
      handleGenerationEvent config `catch` \(e :: SomeException) -> do
        putStrLn $ "[Worker] Error during generation: " ++ show e

-- | Handles the logic for regenerating declarations and the clients.
handleGenerationEvent :: Config.Config -> IO ()
handleGenerationEvent config = do
  findPackageManager >>= \case
    Nothing -> putStrLn "\n[ERROR] Neither 'pnpm' nor 'npm' could be found in your system's PATH.\nPlease install one of them to use the dev watcher.\n"
    Just pm -> do
      putStrLn "[Worker] Regenerating .d.ts files..."
      let pkgMgrCmd = (proc pm ["declarations"]) {cwd = Just $ Config.projectPath config}
      (_, _, _, pkgMgrHandle) <- createProcess pkgMgrCmd
      exitCode <- waitForProcess pkgMgrHandle
      case exitCode of
        ExitSuccess -> do
          putStrLn "[Worker] Declaration files regenerated successfully."
          generateAndWriteCode config
        ExitFailure code ->
          putStrLn $ "[Worker] Error: 'pnpm declarations' failed with exit code " ++ show code

-- | Generates the code and writes it to the target files if it has changed.
generateAndWriteCode :: Config.Config -> IO ()
generateAndWriteCode config = do
  putStrLn "[Worker] Parsing schema and generating clients..."
  projectResult <- P.parseProject (Config.projectPath config ++ "/convex/schema.ts") $ Config.declarationsDir config
  case projectResult of
    Left err ->
      putStrLn $ "[Worker] Error parsing project: " ++ err
    Right project ->
      -- Iterate over each target defined in the config
      forM_ (Config.targetList config) $ \Config.TargetConfig {..} -> do
        putStrLn $ "[Worker] Generating for target: " ++ show lang
        let generatedCode = case lang of
              Config.Python -> Python.generatePythonCode project
              Config.Rust -> Rust.generateRustCode project

        let validatorAction :: (MonadFail m, MonadIO m, Validator.Validator m) => m String
            validatorAction = do
              Validator.setup
              Validator.validate generatedCode >>= \case
                Just formattedAndChecked -> do
                  liftIO $ putStrLn "[Worker] Rust code validation successful."
                  return formattedAndChecked
                Nothing -> do
                  fail "[Worker] Rust code validation failed. Check the output above."

        checkedCode <- case lang of
          Config.Rust -> do
            Rust.Validator.run (Rust.Validator.RustValidatorEnv (Config.validationPath config ++ "/rust_validation_project")) validatorAction
          Config.Python -> do
            -- Python.Validator.run (Python.Validator.PythonValidatorEnv (Config.projectPath config ++ "/python_validation_project")) validatorAction
            return generatedCode

        -- Write to each output file defined for the target
        forM_ output $ \path -> do
          !oldContentResult <- readTargetFileWithRetry path
          putStrLn $ "[Worker] Checking " ++ path ++ " for changes..."
          case oldContentResult of
            Left err -> putStrLn $ "[Worker] Error reading existing file: " ++ show err
            Right oldContent ->
              if checkedCode /= oldContent
                then do
                  putStrLn $ "[Worker]   -> Changes detected for " ++ path
                  putStrLn "[Worker]   -> Writing new content..."
                  writeResult <- writeFileWithRetry path checkedCode
                  case writeResult of
                    Left err -> putStrLn $ "[Worker] Error writing file: " ++ show err
                    Right () -> putStrLn $ "[Worker]   -> Success! Updated " ++ path
                else
                  putStrLn $ "[Worker]   -> No changes detected for " ++ path

-- | Safely reads the content of a file, retrying on busy errors.
readTargetFileWithRetry :: FilePath -> IO (Either IOException String)
readTargetFileWithRetry path = do
  exists <- doesFileExist path
  if exists
    then tryWithRetries 10 200000 $ do
      -- 10 retries, 200ms apart
      content <- readFile path
      _ <- evaluate (rnf content) -- Force evaluation to ensure the file handle is released.
      return content
    else return (Right "")

-- | Writes content to a file, retrying on busy errors.
writeFileWithRetry :: FilePath -> String -> IO (Either IOException ())
writeFileWithRetry path content = tryWithRetries 10 200000 (writeFile path content)

-- | A helper to retry an IO action if it fails due to a "resource busy" error.
tryWithRetries :: Int -> Int -> IO a -> IO (Either IOException a)
tryWithRetries 0 _ action = try action -- Last attempt, return the result
tryWithRetries n delayMicroseconds action = do
  result <- try action
  case result of
    Left e | isAlreadyInUseError e -> do
      putStrLn $ "[Retry] File is busy, retrying in " ++ show (delayMicroseconds `div` 1000) ++ "ms..."
      threadDelay delayMicroseconds
      tryWithRetries (n - 1) delayMicroseconds action
    _ -> return result
