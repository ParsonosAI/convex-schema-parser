{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Backend.Python.Validator (run, PythonValidator (..), PythonValidatorEnv (..)) where

import Control.Monad.Reader
import Convex.Validator
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hGetContents)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)

newtype PythonValidator a = PythonValidator
  { runPythonValidator :: ReaderT PythonValidatorEnv IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader PythonValidatorEnv, MonadFail)

data PythonValidatorEnv = PythonValidatorEnv {projectPath :: FilePath}

run :: PythonValidatorEnv -> PythonValidator a -> IO a
run renv action = runReaderT (runPythonValidator action) renv

instance Validator PythonValidator where
  -- \| Sets up the Python validation sandbox project.
  setup = do
    config <- ask
    let pythonProjectPath = projectPath config
    let stubsPath = pythonProjectPath </> "stubs"

    -- Create the directory structure.
    liftIO $ createDirectoryIfMissing True stubsPath

    -- Write the mypy.ini config file.
    liftIO $
      doesFileExist (pythonProjectPath </> "mypy.ini") >>= \case
        True -> return () -- If it exists, we don't overwrite it.
        False -> writeFile (pythonProjectPath </> "mypy.ini") mypyIniContent

    -- Write the pydantic stub file.
    liftIO $
      doesFileExist (stubsPath </> "pydantic.pyi") >>= \case
        True -> return () -- If it exists, we don't overwrite it.
        False -> writeFile (stubsPath </> "pydantic.pyi") pydanticStubContent

    -- Write the convex stub file.
    liftIO $
      doesFileExist (stubsPath </> "convex.pyi") >>= \case
        True -> return () -- If it exists, we don't overwrite it.
        False -> writeFile (stubsPath </> "convex.pyi") convexStubContent

    -- Write an empty stub for pydantic_core to satisfy mypy
    liftIO $
      doesFileExist (stubsPath </> "pydantic_core.pyi") >>= \case
        True -> return () -- If it exists, we don't overwrite it.
        False -> writeFile (stubsPath </> "pydantic_core.pyi") ""

    return ()

  -- \| Validates the generated Python code using mypy.
  validate generatedCode = do
    pythonProjectPath <- asks projectPath
    let generatedFilePath = pythonProjectPath </> "generated_api.py"

    -- Write the generated code to the sandbox project.
    liftIO $ writeFile generatedFilePath generatedCode

    -- Run `mypy` on the generated file.
    liftIO $ putStrLn "[Validator] Running 'mypy'..."
    -- We run `mypy .` to make it pick up the mypy.ini config automatically.
    let mypyCmd = (proc "mypy" ["."]) {cwd = Just pythonProjectPath, std_out = CreatePipe, std_err = CreatePipe}
    (_, Just hOut, Just hErr, handle) <- liftIO $ createProcess mypyCmd
    exitCode <- liftIO $ waitForProcess handle

    if exitCode == ExitSuccess
      then do
        liftIO $ putStrLn "[Validator] Validation successful."
        content <- liftIO $ readFile generatedFilePath
        return $ Just content
      else do
        liftIO $ putStrLn "[Validator] Error: 'mypy' failed. The generated code has type errors."
        -- Print both stdout and stderr for comprehensive error reporting.
        liftIO $ hGetContents hOut >>= putStrLn
        liftIO $ hGetContents hErr >>= putStrLn
        return Nothing

-- | Content for the mypy.ini configuration file.
mypyIniContent :: String
mypyIniContent =
  unlines
    [ "[mypy]",
      "python_version = 3.9",
      "disallow_untyped_defs = True",
      "check_untyped_defs = True",
      "warn_return_any = True",
      "ignore_missing_imports = True"
    ]

-- | Minimal stub content for the `pydantic` library.
pydanticStubContent :: String
pydanticStubContent =
  unlines
    [ "from typing import Any, Type",
      "",
      "class BaseModel:",
      "    def model_validate(cls: Type, obj: Any) -> Any: ...",
      "    def model_dump_json(self, *, indent: int | None = ...) -> str: ...",
      "",
      "def Field(default: Any = ..., *, alias: str | None = ...) -> Any: ...",
      "",
      "class TypeAdapter:",
      "    def __init__(self, type: Any) -> None: ...",
      "    def validate_python(self, data: Any) -> Any: ...",
      "",
      "class ValidationError(Exception): ..."
    ]

-- | Minimal stub content for the `convex` library.
convexStubContent :: String
convexStubContent =
  unlines
    [ "from typing import Any, Iterator",
      "",
      "class ConvexClient:",
      "    def __init__(self, url: str) -> None: ...",
      "    def set_admin_auth(self, key: str) -> None: ...",
      "    def set_auth(self, token: str) -> None: ...",
      "    def query(self, path: str, args: dict[str, Any]) -> Any: ...",
      "    def mutation(self, path: str, args: dict[str, Any]) -> Any: ...",
      "    def action(self, path: str, args: dict[str, Any]) -> Any: ...",
      "    def subscribe(self, path: str, args: dict[str, Any]) -> Iterator[Any]: ...",
      "",
      "class ConvexError(Exception): ..."
    ]
