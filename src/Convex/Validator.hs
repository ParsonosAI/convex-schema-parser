module Convex.Validator
  ( Validator (..),
    ValidationConfig (..),
  )
where

-- | Configuration for the validation setup process.
newtype ValidationConfig = ValidationConfig
  { -- | The root directory where validator projects will be created.
    --   e.g., "~/.config/convex-schema-parser"
    validationDir :: FilePath
  }

-- | A typeclass for language-specific code validators.
class Validator v where
  -- | Sets up the validation environment (e.g., creates a sandbox project)
  --   and returns a handle to it. This function is idempotent.
  setup :: v ()

  -- | Validates a string of generated code using the provided handle.
  --   Returns the potentially formatted and checked code.
  validate :: String -> v (Maybe String)
