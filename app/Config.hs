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
import Data.Yaml (ParseException, decodeFileEither)
import GHC.Generics (Generic)

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

data Config = Config
  { targets :: [TargetConfig]
  }
  deriving (Show, Generic)

instance FromJSON Config

-- | Loads and parses the YAML configuration file.
loadConfig :: FilePath -> IO (Either ParseException Config)
loadConfig = decodeFileEither
