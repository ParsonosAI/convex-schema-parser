{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Dev (runDevMode, DevOptions (..)) where

import qualified Backend.Python as Python
import qualified Backend.Rust as Rust
import qualified Config
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import Control.DeepSeq (rnf)
import Control.Exception
import Control.Exception (evaluate)
import Control.Monad (forM_, forever, when)
import qualified Convex.Parser as P
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..), exitFailure)
import System.FSNotify
import System.IO.Error (isAlreadyInUseError)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)

data DevOptions = DevOptions
  { convexProjectPath :: FilePath,
    declarationsDir :: FilePath,
    config :: Config.Config
  }

-- | Runs the development mode file watcher with debouncing.
runDevMode :: DevOptions -> IO ()
runDevMode opts = do
  -- TVar to signal that a change has occurred.
  dirtyVar <- newTVarIO True -- Start dirty to trigger an initial run.

  -- Fork a worker thread that handles the generation.
  _ <- forkIO (workerThread dirtyVar opts)

  -- The main thread will watch for file changes.
  withManager $ \mgr -> do
    putStrLn $ "Watching for changes in: " ++ convexPath
    watchDir
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
    convexPath = convexProjectPath opts ++ "/convex"
    showEvent (Added path _ _) = "Added: " ++ path
    showEvent (Modified path _ _) = "Modified: " ++ path
    showEvent (Removed path _ _) = "Removed: " ++ path
    showEvent _ = "Unknown event"

-- | The worker thread loop that performs debouncing.
workerThread :: TVar Bool -> DevOptions -> IO ()
workerThread dirtyVar opts = forever $ do
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
    else handleGenerationEvent opts

-- | Handles the logic for regenerating declarations and the clients.
handleGenerationEvent :: DevOptions -> IO ()
handleGenerationEvent opts@DevOptions {..} = do
  putStrLn "[Worker] Regenerating .d.ts files..."
  let pnpmCmd = (proc "pnpm" ["declarations"]) {cwd = Just convexProjectPath}
  (_, _, _, pnpmHandle) <- createProcess pnpmCmd
  exitCode <- waitForProcess pnpmHandle

  case exitCode of
    ExitSuccess -> do
      putStrLn "[Worker] Declaration files regenerated successfully."
      generateAndWriteCode opts
    ExitFailure code ->
      putStrLn $ "[Worker] Error: 'pnpm declarations' failed with exit code " ++ show code

-- | Generates the code and writes it to the target files if it has changed.
generateAndWriteCode :: DevOptions -> IO ()
generateAndWriteCode DevOptions {..} = do
  putStrLn "[Worker] Parsing schema and generating clients..."
  projectResult <- P.parseProject (convexProjectPath ++ "/convex/schema.ts") declarationsDir
  case projectResult of
    Left err ->
      putStrLn $ "[Worker] Error parsing project: " ++ err
    Right project ->
      -- Iterate over each target defined in the config
      forM_ (Config.targets config) $ \Config.TargetConfig {..} -> do
        putStrLn $ "[Worker] Generating for target: " ++ show lang
        let generatedCode = case lang of
              Config.Python -> Python.generatePythonCode project
              Config.Rust -> Rust.generateRustCode project

        -- Write to each output file defined for the target
        forM_ output $ \path -> do
          !oldContentResult <- readTargetFileWithRetry path
          putStrLn $ "[Worker] Checking " ++ path ++ " for changes..."
          case oldContentResult of
            Left err -> putStrLn $ "[Worker] Error reading existing file: " ++ show err
            Right oldContent ->
              if generatedCode /= oldContent
                then do
                  putStrLn $ "[Worker]   -> Changes detected for " ++ path
                  putStrLn "[Worker]   -> Writing new content..."
                  writeResult <- writeFileWithRetry path generatedCode
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
