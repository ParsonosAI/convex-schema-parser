{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( loadConfig,
    Config (..),
    TargetConfig (..),
    GenTarget (..),
  )
where

import Data.Aeson
import Data.Char (isAlphaNum)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Traversable (forM)
import Data.Yaml (ParseException, decodeFileEither)
import GHC.Generics (Generic)
import System.Directory (getHomeDirectory)
import System.Environment (getEnv, lookupEnv)
import System.FilePath (joinPath, splitPath, (</>))

data GenTarget = Python | Rust
  deriving (Show, Eq, Enum, Bounded, Generic)

-- Make GenTarget parseable from YAML (as a string)
instance FromJSON GenTarget where
  parseJSON = withText "GenTarget" $ \t ->
    case t of
      "Python" -> pure Python
      "Rust" -> pure Rust
      _ -> fail "Unknown target language. Use 'Python' or 'Rust'."

data TargetConfig = TargetConfig
  { lang :: GenTarget,
    output :: [FilePath]
  }
  deriving (Show, Generic)

instance FromJSON TargetConfig

data ConfigRead = ConfigRead
  { project_path :: FilePath,
    declarations_dir :: FilePath,
    targets :: [TargetConfig],
    validation_path :: Maybe (FilePath)
  }
  deriving (Show, Generic)

instance FromJSON ConfigRead

data Config = Config
  { projectPath :: FilePath,
    declarationsDir :: FilePath,
    targetList :: [TargetConfig],
    validationPath :: FilePath
  }
  deriving (Show, Generic)

-- | Loads and parses the YAML configuration file.
loadConfig :: FilePath -> IO (Either ParseException Config)
loadConfig p = do
  r <- decodeFileEither p
  r' <- case r of
    Right config -> do
      cfg <-
        Config
          <$> (expandFullPath $ project_path config)
          <*> (expandFullPath $ declarations_dir config)
          <*> (mapM expandInTargetConfig (targets config))
          <*> (expandFullPath $ maybe "~/.config/convex-schema-parser" id (validation_path config))
      return $ Right cfg
    Left v -> return (Left v :: Either ParseException Config)
  return r'

expandInTargetConfig :: TargetConfig -> IO TargetConfig
expandInTargetConfig (TargetConfig lang outputs) = do
  expandedOutputs <- mapM expandFullPath outputs
  return $ TargetConfig lang expandedOutputs

expandEnvInPath :: FilePath -> IO FilePath
expandEnvInPath input = do
  let components = splitPath input -- preserves trailing slashes
  expanded <- forM components $ \part ->
    case part of
      ('$' : rest) -> do
        let varName = takeWhile isAlphaNum rest
        mVal <- lookupEnv varName
        return $ fromMaybe part mVal
      _ -> return part
  return $ joinPath expanded

expandUserPath :: FilePath -> IO FilePath
expandUserPath path
  | "~/" `isPrefixOf` path = do
      home <- getHomeDirectory
      return $ home </> drop 2 path
  | "$HOME/" `isPrefixOf` path = do
      home <- getEnv "HOME"
      return $ home </> drop 6 path
  | otherwise = return path

expandFullPath :: FilePath -> IO FilePath
expandFullPath path = do
  expandedEnv <- expandEnvInPath path
  expandedUser <- expandUserPath expandedEnv
  return expandedUser
