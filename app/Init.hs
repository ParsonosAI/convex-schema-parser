module Init (runInit) where

defaultConfig :: String
defaultConfig =
  unlines
    [ "# Configuration for the Convex Client Generator.",
      "",
      "# (Required) The absolute path to the root of your Convex project.",
      "# This is the directory that contains the `convex/` folder and `package.json`.",
      "# NOTE: The parser expands `~` and `$HOME` environment variables.",
      "project_path: \"/path/to/your/convex/project\"",
      "",
      "# (Required) The absolute path to the generated TypeScript declarations.",
      "declarations_dir: \"/path/to/your/tmp/declarations\"",
      "",
      "# (Optional) The path where validation sandbox projects will be created.",
      "# Defaults to `~/.config/convex-schema-parser` if omitted.",
      "validation_path: \"~/.config/convex-schema-parser\"",
      "",
      "# (Required) A list of generation targets. You can have one or more.",
      "targets:",
      "  # Example 1: Generate a Rust client for a backend service.",
      "  - lang: Rust",
      "    # A list of one or more output files for this target.",
      "    output:",
      "      - /path/to/my-rust-app/src/convex_api.rs",
      "",
      "  # Example 2: Generate a Python client for data scripts.",
      "  - lang: Python",
      "    output:",
      "      - /path/to/scripts/lib/convex_client.py"
    ]

-- | Creates a default configuration file at the specified path.
runInit :: FilePath -> IO ()
runInit configPath = do
  putStrLn $ "Creating default configuration file at: " ++ configPath
  writeFile configPath defaultConfig
  putStrLn "Configuration file created successfully. Please edit it with your project paths."
  mapM_
    putStrLn
    [ "You will also need a `tsconfig.declarations.json` file next to your `package.json`.",
      "",
      "Example `tsconfig.declarations.json`:",
      "",
      "```",
      "{",
      "  \"compilerOptions\": {",
      "    // --- Output Configuration ---",
      "    \"outDir\": \"./tmp/declarations\", // Output all files to a temporary directory",
      "    \"rootDir\": \"./convex\",          // The root of the source files we care about",
      "",
      "    // --- Generation Settings ---",
      "    \"declaration\": true,            // Generate .d.ts files",
      "    \"emitDeclarationOnly\": true,    // Don't generate any .js files",
      "    \"skipLibCheck\": true,           // Speeds up compilation by not checking library files",
      "",
      "    // --- Module Settings (to match Convex) ---",
      "    \"module\": \"commonjs\",",
      "    \"target\": \"es2020\"",
      "  },",
      "  // Tell tsc where to find the files to compile",
      "  \"include\": [\"convex/**/*.ts\"]",
      "}",
      "```",
      "",
      "Also, adapt your `package.config` with a script that is called by this generator:",
      "",
      "```",
      "  \"scripts\": {",
      "    \"declarations:clean\": \"rm -rf tmp\",",
      "    \"declarations:build\": \"tsc -p tsconfig.declarations.json\",",
      "    \"declarations:copy-generated\": \"cp -r convex/_generated tmp/declarations/\",",
      "    \"declarations\": \"npm run declarations:clean && npm run declarations:build && npm run declarations:copy-generated\",",
      "  },",
      "```",
      "",
      "If that is done, you are all set to use `dev`. (:"
    ]
